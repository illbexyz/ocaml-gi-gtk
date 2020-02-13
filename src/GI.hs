{-# LANGUAGE NamedFieldPuns #-}
module GI
  ( genBindings
  , Library(..)
  , resolveRecursion
  )
where

import           Control.Monad
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map                      as M

import           System.Directory               ( createDirectoryIfMissing
                                                , doesDirectoryExist
                                                )
import           System.FilePath

import           Data.GI.CodeGen.CabalHooks     ( TaggedOverride(..) )
import           Data.GI.CodeGen.LibGIRepository
                                                ( setupTypelibSearchPath )
import           Data.Bifunctor                 ( bimap )

import           API                            ( loadGIRInfo
                                                , API
                                                , Name(..)
                                                , GIRInfo(..)
                                                )
import           Config                         ( Config(..) )

import           Overrides                      ( parseOverrides
                                                , girFixups
                                                , filterAPIsAndDeps
                                                )
import           Code                           ( evalCodeGen
                                                , writeModuleTree
                                                , ModuleInfo
                                                , CodeGen
                                                )
import           CodeGen                        ( genModule )
import           Dune
import           ModulePath                     ( toModulePath
                                                , ModulePath(..)
                                                )
import           Naming                         ( escapeOCamlReserved
                                                , camelCaseToSnakeCase
                                                )
import           Util                           ( utf8ReadFile
                                                , utf8WriteFile
                                                , copyDirectory
                                                , removeDirectoryContents
                                                )


data Library = Library {
    name :: Text,
    version :: Text,
    overridesFile :: Maybe FilePath
}

-- | Like `evalCodeGen`, but discard the resulting output value.
genCode :: Config -> M.Map Name API -> ModulePath -> CodeGen () -> ModuleInfo
genCode cfg apis mPath cg = snd $ evalCodeGen cfg apis mPath cg

-- | Generate the code for the given module.
genModuleCode
  :: Text -- ^ name
  -> Text -- ^ version
  -> Bool -- ^ verbose
  -> [TaggedOverride] -- ^ Explicit overrides
  -> IO (ModuleInfo, [Text])
genModuleCode name version verbosity overrides = do
  setupTypelibSearchPath []

  parsed <- forM overrides $ \(TaggedOverride tag ovText) ->
    parseOverrides ovText >>= \case
      Left err ->
        error
          $  "Error when parsing overrides file \""
          <> T.unpack tag
          <> "\":"
          <> T.unpack err
      Right ovs -> return ovs

  let ovs = mconcat parsed

  (gir, girDeps) <- loadGIRInfo verbosity name (Just version) [] (girFixups ovs)
  let dependencies = girNSName <$> girDeps
      (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs      = M.union apis deps
      cfg = Config { modName = name, verbose = verbosity, overrides = ovs }

  return
    (genCode cfg allAPIs (toModulePath name) (genModule apis), dependencies)

-- | Write a module containing information about the configuration for
-- the package.
genConfigFiles :: Maybe FilePath -> Text -> Maybe TaggedOverride -> IO ()
genConfigFiles outputDir modName _maybeGiven = do
  let baseDir      = joinPath [fromMaybe "" outputDir]
      dunePrj      = joinPath [baseDir, "dune-project"]
      duneBindings = joinPath [baseDir, "dune"]
      dirname      = takeDirectory baseDir

  createDirectoryIfMissing True dirname

  utf8WriteFile dunePrj $ T.unlines
    ["(lang dune 2.0)", "(package", " (name GI" <> modName <> "))"]

  utf8WriteFile duneBindings $ T.unlines
    ["(env", " (_", "  (binaries", "   ./tools/dune_config.exe)))"]

genBindings :: Bool -> Library -> IO ()
genBindings verbosity Library { GI.name = libName, version, overridesFile } =
  do
    let inheritedOverrides = []
        outputDir          = "bindings" </> T.unpack libName

    dirExists <- doesDirectoryExist outputDir
    when dirExists $ removeDirectoryContents outputDir

    givenOvs <- traverse
      (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname)
      overridesFile

    let ovs = maybe inheritedOverrides (: inheritedOverrides) givenOvs

    (m, deps') <- genModuleCode libName version verbosity ovs

    let deps = filter (`notElem` ["xlib", "GModule"]) deps'

    _ <- writeModuleTree verbosity (Just outputDir) m deps

    genConfigFiles (Just outputDir) libName givenOvs

    copyDirectory "base-ocaml/tools" (outputDir </> "tools")

    putStrLn $ "Compiling " <> outputDir <> " using Dune..."

    mbDuneError <- compile outputDir
    forM_ mbDuneError $ \duneError -> do
      let files = parseCycles duneError
      if null files
        then putStrLn duneError
        else do
          putStrLn $ "Found a cycle in the following files: " <> T.unpack
            (T.intercalate ", " files)
          putStrLn
            $  "They will be moved to "
            <> (outputDir </> T.unpack libName </> "Recursion.ml")
          resolveRecursion (outputDir </> T.unpack libName) files
          putStrLn "Recompiling..."
          cycleError <- compile outputDir
          case cycleError of
            Nothing  -> putStrLn "Success"
            Just err -> putStrLn err

resolveRecursion :: FilePath -> [Text] -> IO ()
resolveRecursion dirPrefix files = do
  parsedClassesDecls <- forM files $ \file -> do
    fileContent <- utf8ReadFile (dirPrefix </> T.unpack file <> ".ml")

    let (_opens , rest) = parseOpens fileContent
        (classes, decl) = parseClasses rest

    return (classes, decl)

  let classesRaw = map fst parsedClassesDecls
      classesAnd =
        head classesRaw <> T.concat (drop 1 $ map classToAnd classesRaw)
      classes = removeRefs files classesAnd

      decls   = T.concat $ map snd parsedClassesDecls

  let newFilesContent = map fileToContent files

  utf8WriteFile (dirPrefix </> "Recursion.ml")
                (T.unlines ["open Gobject", classes, decls])
  forM_ (zip files newFilesContent) $ \(file, content) ->
    utf8WriteFile (dirPrefix </> T.unpack file <> ".ml") content

 where
  classToAnd :: Text -> Text
  classToAnd = T.replace "class " "and "

  removeRefs :: [Text] -> Text -> Text
  removeRefs fs classes =
    foldl (\acc moduleName -> T.replace (moduleName <> ".") "" acc) classes fs

  fileToContent :: Text -> Text
  fileToContent x = do
    let removeG   = fst $ T.splitAt (T.length x - 1) x
        ocamlName = escapeOCamlReserved $ camelCaseToSnakeCase removeG
    T.unlines
      [ "class " <> ocamlName <> "_skel = Recursion." <> ocamlName <> "_skel"
      , "class "
      <> ocamlName
      <> "_signals = Recursion."
      <> ocamlName
      <> "_signals"
      , "class " <> ocamlName <> " = Recursion." <> ocamlName
      , "let " <> ocamlName <> " = Recursion." <> ocamlName
      ]

parseOpens :: Text -> (Text, Text)
parseOpens t = join bimap T.unlines (splitAt 2 $ T.lines t)

parseClasses :: Text -> (Text, Text)
parseClasses = T.breakOn "let "
