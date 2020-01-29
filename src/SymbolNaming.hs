{-# LANGUAGE ViewPatterns #-}
module SymbolNaming
  ( lowerName
  , lowerSymbol
  , upperName
  , escapedArgName
  , escapeOCamlReserved
  , splitCamelCase
  , hyphensToCamelCase
  , underscoresToCamelCase
  , underscoresToHyphens
  , hyphensToUnderscores
  , camelCaseToSnakeCase
  , signalOCamlName
  , signalHaskellName
  , submoduleLocation
  , qualifiedAPI
  , qualifiedSymbol
  , mlGiPrefix
  , enumVal
  , flagsVal
  , optFlagsVal
  , valEnum
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Char                     as C

import           API
import           Type                           ( Type(TInterface) )

import           Code                           ( CodeGen
                                                , group
                                                , line
                                                , exportDecl
                                                , qualified
                                                , getAPI
                                                )
import           ModulePath                     ( ModulePath
                                                , (/.)
                                                , toModulePath
                                                )
import           Util                           ( lcFirst
                                                , ucFirst
                                                , modifyQualified
                                                )


-- | Move leading underscores to the end.
--
-- === Examples
-- >>> sanitize "_Value_Data_Union"
-- "Value_Data_Union_"
sanitize :: Text -> Text
sanitize (T.uncons -> Just ('_', xs)) = sanitize xs <> "_"
sanitize xs                           = xs

-- | Same as `lowerSymbol`, but accepts a `Name`. The namespace part
-- of the name will be discarded.
--
-- === __Examples__
-- >>> lowerName (Name "Gtk" "main_quit")
-- "mainQuit"
lowerName :: Name -> Text
lowerName (Name _ s) = lowerSymbol s

-- | Turn the given identifier into camelCase, starting with a
-- lowercase letter.
--
-- === __Examples__
-- >>> lowerSymbol "main_quit"
-- "mainQuit"
lowerSymbol :: Text -> Text
lowerSymbol s = case underscoresToCamelCase (sanitize s) of
  "" -> error "empty name!!"
  n  -> lcFirst n

-- | Turn the given `Name` into CamelCase, starting with a capital letter.
--
-- === __Examples__
-- >>> upperName (Name "Foo" "bar_baz")
-- "BarBaz"
upperName :: Name -> Text
upperName (Name _ s) = underscoresToCamelCase (sanitize s)

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
submoduleLocation n (APIUnion     _) = "Unions" /. upperName n

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

underscoresToHyphens :: Text -> Text
underscoresToHyphens = T.replace "_" "-"

hyphensToUnderscores :: Text -> Text
hyphensToUnderscores = T.replace "-" "_"

camelCaseToSnakeCase :: Text -> Text
camelCaseToSnakeCase = T.concatMap f . lcFirst
 where
  f c | C.isUpper c = "_" <> T.toLower (T.singleton c)
      | otherwise   = T.singleton c

splitCamelCase :: Text -> [Text]
splitCamelCase t = map T.pack $ splitCamelCase' (T.unpack t) []
 where
  splitCamelCase' ""       []                = []
  splitCamelCase' ""       acc               = reverse acc
  splitCamelCase' (x : xs) []                = splitCamelCase' xs [[x]]
  splitCamelCase' (x : xs) acc | C.isUpper x = splitCamelCase' xs ([x] : acc)
  splitCamelCase' (x : xs) (a : as) = splitCamelCase' xs ((a ++ [x]) : as)

-- | Turn a hyphen-separated identifier into camel case.
--
-- === __Examples__
-- >>> hyphensToCamelCase "one-sample-string"b
-- "OneSampleString"
hyphensToCamelCase :: Text -> Text
hyphensToCamelCase = T.concat . map ucFirst . T.split (== '-')

-- | Similarly to `hyphensToCamelCase`, turn a name
-- separated_by_underscores into CamelCase. We preserve final and
-- initial underscores, and n>1 consecutive underscores are
-- transformed into n-1 underscores.
--
-- === __Examples__
-- >>> underscoresToCamelCase "sample_id"
-- "SampleId"
--
-- >>> underscoresToCamelCase "_internal_id_"
-- "_InternalId_"
--
-- >>> underscoresToCamelCase "multiple___underscores"
-- "Multiple__Underscores"
underscoresToCamelCase :: Text -> Text
underscoresToCamelCase = T.concat . map normalize . map ucFirst . T.split
  (== '_')
 where
  normalize :: Text -> Text
  normalize "" = "_"
  normalize s  = s

-- | Name for the given argument, making sure it is a valid Haskell
-- argument name (and escaping it if not).
escapedArgName :: Arg -> Text
escapedArgName arg
  | "_" `T.isPrefixOf` argCName arg
  = argCName arg
  | otherwise
  = escapeReserved . lcFirst . underscoresToCamelCase . argCName $ arg

escapeOCamlReserved :: Text -> Text
escapeOCamlReserved "unit"   = "unit_"
escapeOCamlReserved "object" = "object_"
escapeOCamlReserved "begin"  = "begin_"
escapeOCamlReserved "end"    = "end_"
escapeOCamlReserved t        = do
  let (nums, text) = T.span C.isNumber t
  text <> nums

-- | Reserved symbols, either because they are Haskell syntax or
-- because the clash with symbols in scope for the generated bindings.
escapeReserved :: Text -> Text
escapeReserved "type"         = "type_"
escapeReserved "in"           = "in_"
escapeReserved "data"         = "data_"
escapeReserved "instance"     = "instance_"
escapeReserved "where"        = "where_"
escapeReserved "module"       = "module_"
-- Reserved because we generate code that uses these names.
escapeReserved "result"       = "result_"
escapeReserved "return"       = "return_"
escapeReserved "show"         = "show_"
escapeReserved "fromEnum"     = "fromEnum_"
escapeReserved "toEnum"       = "toEnum_"
escapeReserved "undefined"    = "undefined_"
escapeReserved "error"        = "error_"
escapeReserved "map"          = "map_"
escapeReserved "length"       = "length_"
escapeReserved "mapM"         = "mapM__"
escapeReserved "mapM_"        = "mapM___"
escapeReserved "fromIntegral" = "fromIntegral_"
escapeReserved "realToFrac"   = "realToFrac_"
escapeReserved "peek"         = "peek_"
escapeReserved "poke"         = "poke_"
escapeReserved "sizeOf"       = "sizeOf_"
escapeReserved "when"         = "when_"
escapeReserved "default"      = "default_"
escapeReserved s | "set_" `T.isPrefixOf` s = s <> "_"
                 | "get_" `T.isPrefixOf` s = s <> "_"
                 | otherwise               = s

-- | Return the name for the signal in Haskell CamelCase conventions.
signalHaskellName :: Text -> Text
signalHaskellName sn =
  let (w : ws) = T.split (== '-') sn in w <> T.concat (map ucFirst ws)

signalOCamlName :: Text -> Text
signalOCamlName = hyphensToUnderscores

mlGiPrefix :: Name -> Text -> Text
mlGiPrefix nm t = "ml_gi" <> T.toLower (namespace nm) <> "_" <> t

cGIPrefix :: Text
cGIPrefix = "GI_"

enumVal :: Name -> Text
enumVal (Name ns n) = cGIPrefix <> ns <> n <> "_val"

flagsVal :: Name -> Text
flagsVal n = "Flags_" <> enumVal n

optFlagsVal :: Name -> Text
optFlagsVal n = "Opt" <> flagsVal n

valEnum :: Name -> Text
valEnum (Name ns n) = cGIPrefix <> "Val_" <> ns <> n
