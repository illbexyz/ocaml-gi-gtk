{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( genBindings
    , Library(..)
    )
where

import           Control.Monad                  ( forM
                                                , forM_
                                                , void
                                                , when
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map                      as M

import           System.Directory               ( createDirectoryIfMissing
                                                , removeDirectoryRecursive
                                                , doesDirectoryExist
                                                )
import           System.FilePath

import           Data.GI.CodeGen.CabalHooks     ( TaggedOverride(..) )
import           Data.GI.CodeGen.LibGIRepository
                                                ( setupTypelibSearchPath )

import           API                            ( loadGIRInfo
                                                , API
                                                , Name(..)
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
import           ModulePath                     ( toModulePath
                                                , ModulePath(..)
                                                )
import           Util                           ( utf8ReadFile
                                                , utf8WriteFile
                                                , copyDirectory
                                                , removeDirectoryContents
                                                )

import           Debug.Trace

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
    -> IO ModuleInfo
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

    (gir, girDeps) <- loadGIRInfo verbosity
                                  name
                                  (Just version)
                                  []
                                  (girFixups ovs)
    let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
        allAPIs = M.union apis deps
        cfg = Config { modName = name, verbose = verbosity, overrides = ovs }

    return $ genCode cfg allAPIs (toModulePath name) (genModule apis)

-- | Write a module containing information about the configuration for
-- the package.
genConfigFiles :: Maybe FilePath -> Text -> Maybe TaggedOverride -> IO ()
genConfigFiles outputDir modName _maybeGiven = do
    let baseDir      = joinPath [fromMaybe "" outputDir]
        dunePrj      = joinPath [baseDir, "dune-project"]
        duneBindings = joinPath [baseDir, "dune"]
        duneModule   = joinPath [baseDir, T.unpack modName, "dune"]
        dirname      = takeDirectory baseDir

    createDirectoryIfMissing True dirname

    utf8WriteFile dunePrj $ T.unlines
        ["(lang dune 2.0)", "(package", " (name GI" <> modName <> "))"]

    utf8WriteFile duneBindings $ T.unlines
        ["(env", " (_", "  (binaries", "   ./tools/dune_config.exe)))"]

    -- utf8WriteFile duneModule $ T.unlines
    --     [ "(library"
    --     , " (name " <> modName <> ")"
    --     , " (libraries objects enums)"
    --     , ")"
    --     ]


genBindings :: Bool -> Library -> IO ()
genBindings verbosity Library { Lib.name, version, overridesFile } = do
    -- let name               = "Gdk"
    --     overridesFile      = Just $ "overrides" </> "Gdk.overrides"
    let inheritedOverrides = []
        outputDir          = "bindings" </> T.unpack name

    dirExists <- doesDirectoryExist outputDir
    when dirExists $ removeDirectoryContents outputDir

    givenOvs <- traverse
        (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname)
        overridesFile

    let ovs = maybe inheritedOverrides (: inheritedOverrides) givenOvs

    m          <- genModuleCode name version verbosity ovs
    (_, cDirs) <- writeModuleTree verbosity (Just outputDir) m

    genConfigFiles (Just outputDir) name givenOvs

    copyDirectory "base-ocaml/tools" (outputDir </> "tools")
    -- copyDirectory "examples"         (outputDir </> "examples")

    -- forM_ cDirs $ \dir -> copyDirectory "base-ocaml/c" dir

    -- return ()
