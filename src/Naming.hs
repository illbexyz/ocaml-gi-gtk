{-# LANGUAGE ViewPatterns #-}
module Naming
  ( lowerName
  , lowerSymbol
  , upperName
  , escapeOCamlReserved
  , splitCamelCase
  , hyphensToCamelCase
  , underscoresToCamelCase
  , underscoresToHyphens
  , hyphensToUnderscores
  , camelCaseToSnakeCase
  , ocamlIdentifier
  , nsOCamlIdentifier
  , nsOCamlType
  , signalOCamlName
  , mlGiPrefix
  , enumVal
  , flagsVal
  , optFlagsVal
  , valEnum
  , interfaceVal
  , objectVal
  , valInterface
  , valObject
  , valOptInterface
  , valOptObject
  , valStruct
  , structVal
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Char                     as C

import           API
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

escapeOCamlReserved :: Text -> Text
escapeOCamlReserved "unit"   = "unit_"
escapeOCamlReserved "object" = "object_"
escapeOCamlReserved "begin"  = "begin_"
escapeOCamlReserved "end"    = "end_"
escapeOCamlReserved "done"   = "done_"
escapeOCamlReserved "type"   = "type_"
escapeOCamlReserved "new"    = "new_"
escapeOCamlReserved t        = do
  let (nums, text) = T.span C.isNumber t
  text <> nums

ocamlIdentifier :: Name -> Text
ocamlIdentifier (Name _ nm) = escapeOCamlReserved $ camelCaseToSnakeCase nm

nsOCamlIdentifier :: Text -> Name -> Text
nsOCamlIdentifier nspace n@(Name ns nm) | nspace == ns = ocamlIdentifier n
nsOCamlIdentifier nspace n@(Name ns nm) =
  "GI" <> ns <> ".Types." <> ocamlIdentifier n

nsOCamlType :: Text -> Name -> Text
nsOCamlType currNs n@(Name ns _) | currNs == ns = "Types." <> ocamlIdentifier n
nsOCamlType currNs n@(Name ns nm) =
  "GI" <> ns <> ".Types." <> ocamlIdentifier n

signalOCamlName :: Text -> Text
signalOCamlName = escapeOCamlReserved . hyphensToUnderscores

mlGiPrefix :: Name -> Text -> Text
mlGiPrefix n t = "ml_gi" <> T.toLower (namespace n) <> "_" <> t

cGIPrefix :: Text
cGIPrefix = "GI_"

enumVal :: Name -> Text
enumVal (Name ns nm) = cGIPrefix <> ns <> nm <> "_val"

flagsVal :: Name -> Text
flagsVal n = "Flags_" <> enumVal n

optFlagsVal :: Name -> Text
optFlagsVal n = "Opt" <> flagsVal n

valEnum :: Name -> Text
valEnum (Name ns nm) = cGIPrefix <> "Val_" <> ns <> nm

interfaceVal :: Name -> Text
interfaceVal (Name ns nm) = ns <> nm <> "_val"

valInterface :: Name -> Text
valInterface (Name ns nm) = "Val_" <> ns <> nm

objectVal :: Name -> Text
objectVal = interfaceVal

valObject :: Name -> Text
valObject = valInterface

valOptInterface :: Name -> Text
valOptInterface n = "Opt" <> valInterface n

valOptObject :: Name -> Text
valOptObject = valOptInterface

structVal :: Name -> Text
structVal = interfaceVal

valStruct :: Name -> Text
valStruct = valInterface
