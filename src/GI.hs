{-# LANGUAGE NamedFieldPuns #-}
module GI
  ( genBindings
  , Library(..)
  , resolveRecursion
  )
where

import           Control.Monad
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )
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
import           Data.Bifunctor                 ( bimap
                                                , first
                                                )
import           Data.List                      ( elemIndex
                                                , partition
                                                )

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

    -- Need to compile twice to find all the cycles (no idea why)
    mbDuneError' <- compile outputDir
    forM_ mbDuneError' $ \duneError -> do
      putStrLn "Dune's error message:\n"
      putStrLn $ unlines $ map ("    " <>) $ lines duneError
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
  parsedDecls <- forM files $ \file -> do
    fileContent <- utf8ReadFile (dirPrefix </> T.unpack file <> ".ml")

    let (_opens , rest) = parseOpens fileContent
        (classes, _   ) = parseClasses rest

    return classes

  let allDecls        = concat parsedDecls
      classes         = cTextToText files allDecls
      -- classes         = ""
      newFilesContent = map cTextToRecursion parsedDecls
      -- decls           = T.concat $ map snd parsedDecls
  utf8WriteFile (dirPrefix </> "Recursion.ml") classes
  forM_ (zip files newFilesContent) $ \(file, content) ->
    utf8WriteFile (dirPrefix </> T.unpack file <> ".ml") content
 where
  classToAnd :: Decl -> Decl
  classToAnd ct = ct { declText = T.replace "class " "and " (declText ct) }

  removeRefs :: [Text] -> Decl -> Decl
  removeRefs fs cText = foldl
    (\acc moduleName ->
      acc { declText = T.replace (moduleName <> ".") "" (declText acc) }
    )
    cText
    fs

  cTextToText :: [Text] -> [Decl] -> Text
  cTextToText _  []     = ""
  cTextToText fs ctexts = do
    let (signals, rest) = partition
          (\Decl { declName } -> "_signals" `T.isSuffixOf` declName)
          ctexts
        (lets, skels) = partition ((== Let) . declType) rest
        skels'        = head skels : drop 1 (map classToAnd skels)
    T.unlines
      $  (declText <$> signals)
      <> (declText . removeRefs fs <$> skels')
      <> (declText . removeRefs fs <$> lets)

  cTextToRecursion :: [Decl] -> Text
  cTextToRecursion cTexts = T.unlines $ map inner cTexts
   where
    inner Decl { declType = Class True, declName } =
      "class virtual " <> declName <> " = Recursion." <> declName
    inner Decl { declType = Class False, declName }
      | "_skel" `T.isSuffixOf` declName
      = "class " <> declName <> " = Recursion." <> declName
      | "_signals" `T.isSuffixOf` declName
      = "class " <> declName <> " = Recursion." <> declName
      | otherwise
      = "class " <> declName <> " = Recursion." <> declName
    inner Decl { declType = Let, declName } =
      "let " <> declName <> " = Recursion." <> declName

parseOpens :: Text -> (Text, Text)
parseOpens t
  | "open " `T.isPrefixOf` t = join bimap T.unlines (splitAt 2 $ T.lines t)
  | otherwise                = ("", t)

data DeclType = Class Bool  -- ^ is a virtual class
              | Let
              deriving (Eq, Show)
data Decl = Decl { declType :: DeclType, declName :: Text, declText :: Text } deriving (Show)

parseClasses :: Text -> ([Decl], Text)
parseClasses = parseClasses' []
 where
  parseClasses' :: [Decl] -> Text -> ([Decl], Text)
  parseClasses' acc t = do
    let (mbCText, rest) = parseClass t
    case mbCText of
      Nothing       -> (acc, rest)
      Just declText -> parseClasses' (acc ++ [declText]) rest

parseClass :: Text -> (Maybe Decl, Text)
parseClass t
  | "class virtual " `T.isPrefixOf` t = handleClass
    True
    (fromJust $ T.stripPrefix "class virtual " t)
  | "class " `T.isPrefixOf` t = handleClass
    False
    (fromJust $ T.stripPrefix "class " t)
  | "and " `T.isPrefixOf` t = (handleClass False)
    (fromJust $ T.stripPrefix "and " t)
  | "let " `T.isPrefixOf` t = handleLet (fromJust $ T.stripPrefix "let " t)
  | otherwise = (Nothing, t)
 where
  handleClass isVirtual txt = first
    (Just . Decl (Class isVirtual) (T.strip . fst $ T.breakOn "obj " txt))
    (parseClass' $ T.lines t)
  handleLet txt = first
    (Just . Decl Let (T.strip . fst $ T.breakOn " = begin" txt))
    (parseClass' $ T.lines t)

  parseClass' :: [Text] -> (Text, Text)
  parseClass' ts = do
    let idx = maybe 0 (+ 1) (elemIndex "end" ts)
    bimap T.unlines (T.stripStart . T.unlines) (splitAt idx ts)
