-- | Support for enums and flags.
module EnumFlags
  ( genEnum
  , genFlags
  , hashVariant
  )
where

import           Control.Monad                  ( when
                                                , forM_
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Char                      ( ord )
import           Data.Bits
import qualified Data.Text                     as T

import           Foreign.C                      ( CUInt )
import           Foreign.Storable               ( sizeOf )

import           API
import           Code
import           SymbolNaming                   ( upperName
                                                , camelCaseToSnakeCase
                                                , escapeOCamlReserved
                                                , mlGiPrefix
                                                )
import           Util                           ( tshow )

data EnumOrFlag = Enum
                | Flag
                 deriving (Show, Eq, Ord)

-- OCaml's way to represent polymorphic variants in C
hashVariant :: String -> Int
hashVariant = toSigned64 . reduceTo31bits . variantHash
 where
  variantHash' xs acc = foldl (\a c -> (223 * a) + ord c) acc xs
  variantHash xs = variantHash' xs 0
  reduceTo31bits hash = hash .&. ((1 `shiftL` 31) - 1)
  toSigned64 hash = if hash > 0x3FFFFFFF then hash - (1 `shiftL` 31) else hash

genEnumOrFlags
  :: HaddockSection -> Name -> Enumeration -> EnumOrFlag -> ExcCodeGen ()
genEnumOrFlags _docSection n@(Name ns _name) e enumOrFlag = do
  -- Conversion functions expect enums and flags to map to CUInt,
  -- which we assume to be of 32 bits. Fail early, instead of giving
  -- strange errors at runtime.
  when (sizeOf (0 :: CUInt) /= 4)
    $  notImplementedError
    $  "Unsupported CUInt size: "
    <> tshow (sizeOf (0 :: CUInt))
  when (enumStorageBytes e /= 4)
    $  notImplementedError
    $  "Storage of size /= 4 not supported : "
    <> tshow (enumStorageBytes e)

  let
    enumName = escapeOCamlReserved $ camelCaseToSnakeCase $ upperName n
    memberNames =
      map (escapeOCamlReserved . T.toUpper . enumMemberName) (enumMembers e)
    variants    = map ("`" <>) memberNames
    cIds        = map enumMemberCId (enumMembers e)
    namesAndIds = zip memberNames cIds
    mlTableName = mlGiPrefix n ("table_" <> enumName)
    ocamlTbl    = enumName <> "_tbl"
    cGetterFn =
      mlGiPrefix n $ T.toLower (namespace n) <> "_get_" <> enumName <> "_table"

  forM_ memberNames $ \memberName -> do
    let hashValue = T.pack $ show $ hashVariant $ T.unpack memberName
    hline
      $  "#define MLTAG_"
      <> memberName
      <> " ((value)("
      <> hashValue
      <> "*2+1))"

  line $ "type " <> enumName <> " = [ " <> T.intercalate " | " variants <> " ]"
  blank
  line
    $  "external get_"
    <> enumName
    <> "_table : unit -> "
    <> enumName
    <> " Gpointer.variant_table = \""
    <> cGetterFn
    <> "\""
  line $ "let " <> ocamlTbl <> " = get_" <> enumName <> "_table ()"
  if enumOrFlag == Enum
    then line $ "let " <> enumName <> " = Gobject.Data.enum " <> ocamlTbl
    else line $ "let " <> enumName <> " = Gobject.Data.flags " <> ocamlTbl
  blank

  addCDep $ ns <> "Enums"

  cline $ "const lookup_info " <> mlTableName <> "[] = {"
  cline $ "  { 0, " <> T.pack (show (length (enumMembers e))) <> " },"
  forM_ namesAndIds $ \(memberName, memberCId) ->
    cline $ "  { MLTAG_" <> memberName <> ", " <> memberCId <> " },"
  cline "};"
  cline ""
  cline $ "CAMLprim value " <> cGetterFn <> " () {"
  cline $ "  return (value) " <> mlTableName <> ";"
  cline "}"
  cline ""

genEnum :: Name -> Enumeration -> CodeGen ()
genEnum n@(Name _ _name) enum = do
  -- line $ "-- Enum " <> name

  let docSection = NamedSubsection EnumSection (upperName n)

  handleCGExc
    (\e -> commentLine $ "Could not generate: " <> describeCGError e)
    (genEnumOrFlags docSection n enum Enum)

-- | Very similar to enums, but we also declare ourselves as members of
-- the IsGFlag typeclass.
genFlags :: Name -> Flags -> CodeGen ()
genFlags n@(Name _ name) (Flags enum) = do
  -- line $ "-- Flags " <> name

  let docSection = NamedSubsection FlagSection (upperName n)
  handleCGExc
    (\e -> commentLine $ "Could not generate: " <> describeCGError e)
    (genEnumOrFlags docSection n enum Flag)
