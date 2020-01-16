-- | Support for enums and flags.
module EnumFlags
  ( genEnum
  , genFlags
  )
where

import           Control.Monad                  ( when )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T

import           Foreign.C                      ( CUInt )
import           Foreign.Storable               ( sizeOf )

import           API
import           Code
import           SymbolNaming                   ( upperName
                                                , camelCaseToSnakeCase
                                                )
import           Util                           ( tshow )

genEnumOrFlags :: HaddockSection -> Name -> Enumeration -> ExcCodeGen ()
genEnumOrFlags _docSection n@(Name ns _name) e = do
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

  let enumName  = camelCaseToSnakeCase $ T.toLower ns <> upperName n
  let enumMembs = map (("`" <>) . T.toUpper . enumMemberName) (enumMembers e)

  line $ "type " <> enumName <> " = [ " <> T.intercalate " | " enumMembs <> " ]"

genEnum :: Name -> Enumeration -> CodeGen ()
genEnum n@(Name _ _name) enum = do
  -- line $ "-- Enum " <> name

  let docSection = NamedSubsection EnumSection (upperName n)

  handleCGExc
    (\e -> commentLine $ "Could not generate: " <> describeCGError e)
    (genEnumOrFlags docSection n enum)

-- | Very similar to enums, but we also declare ourselves as members of
-- the IsGFlag typeclass.
genFlags :: Name -> Flags -> CodeGen ()
genFlags n@(Name _ name) (Flags enum) = do
  line $ "-- Flags " <> name

  let docSection = NamedSubsection FlagSection (upperName n)
  handleCGExc
    (\e -> commentLine $ "Could not generate: " <> describeCGError e)
    (genEnumOrFlags docSection n enum)
