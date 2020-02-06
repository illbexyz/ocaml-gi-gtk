module QualifiedNaming
  ( submoduleLocation
  , qualifiedAPI
  , qualifiedSymbol
  , signalHaskellName
  , escapedArgName
  , nsOCamlClass
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           API
import           Code                           ( CodeGen
                                                , qualified
                                                , getAPI
                                                , currentNS
                                                , currentModule
                                                )
import           ModulePath                     ( ModulePath
                                                , (/.)
                                                , toModulePath
                                                )
import           Naming
import           Util                           ( lcFirst
                                                , ucFirst
                                                )

-- | Return the name for the signal in Haskell CamelCase conventions.
signalHaskellName :: Text -> Text
signalHaskellName sn =
  let (w : ws) = T.split (== '-') sn in w <> T.concat (map ucFirst ws)

-- | Construct the submodule path where the given API element will
-- live. This is the path relative to the root for the corresponding
-- namespace. I.e. the "GI.Gtk" part is not prepended.
submoduleLocation :: Name -> API -> ModulePath
submoduleLocation _ (APIConst     _) = "Constants"
submoduleLocation _ (APIFunction  _) = "Functions"
submoduleLocation _ (APICallback  _) = "Callbacks"
submoduleLocation n (APIEnum      _) = "Enums"
submoduleLocation n (APIFlags     _) = "Enums"
submoduleLocation n (APIInterface _) = "" /. upperName n
submoduleLocation n (APIObject    _) = "" /. upperName n
submoduleLocation n (APIStruct    _) = "" /. upperName n
submoduleLocation n (APIUnion     _) = "" /. upperName n

-- | Return an identifier for the given interface type valid in the current
-- module.
qualifiedAPI :: Name -> CodeGen Text
qualifiedAPI n@(Name ns _) = do
  api <- getAPI (TInterface n)
  qualified (toModulePath (ucFirst ns) <> submoduleLocation n api) n

-- | Construct an identifier for the given symbol in the given API.
qualifiedSymbol :: Text -> Name -> CodeGen Text
qualifiedSymbol s n@(Name ns _) = do
  api <- getAPI (TInterface n)
  qualified (toModulePath (ucFirst ns) <> submoduleLocation n api)
            (Name ns (camelCaseToSnakeCase s))

-- | Name for the given argument, making sure it is a valid Haskell
-- argument name (and escaping it if not).
escapedArgName :: Arg -> Text
escapedArgName arg
  | "_" `T.isPrefixOf` argCName arg
  = argCName arg
  | otherwise
  = escapeOCamlReserved . lcFirst . underscoresToCamelCase . argCName $ arg

nsOCamlClass :: Name -> CodeGen Text
nsOCamlClass (  Name "Gtk" "Widget") = return "GObj.widget"
-- nsOCamlClass (  Name "Gdk" "Window") = return "GWindow.window"
nsOCamlClass n@(Name ns    nm      ) = do
  currNs  <- currentNS
  currMod <- currentModule
  return $ case (currNs == ns, currMod == nm) of
    (True , True ) -> ocamlIdentifier n
    (True , False) -> name n <> "G." <> ocamlIdentifier n
    (False, _    ) -> "GI" <> ns <> "." <> name n <> "G." <> ocamlIdentifier n
