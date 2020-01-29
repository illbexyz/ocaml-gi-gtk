module Conversions
  ( callableHasClosures
  , haskellType
  , ExposeClosures(..)
  , typeIsCallback
  , typeAllocInfo
  , TypeAllocInfo(..)
  , outParamOcamlType
  , ocamlDataConv
  , ocamlValueToC
  , cToOCamlValue
  , cType
  )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , pure
                                                , Applicative
                                                )
import           Control.Monad                  ( when
                                                , unless
                                                )
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Exts                       ( IsString(..) )

import           Foreign.C.Types                ( CInt
                                                , CUInt
                                                )
import           Foreign.Storable               ( sizeOf )

import           API

import           Type
import           Code
import           GObject
import           SymbolNaming
import           Util

import           Debug.Trace


-- | Whether to expose closures and the associated destroy notify
-- handlers in the Haskell wrapper.
data ExposeClosures = WithClosures
                    | WithoutClosures
  deriving (Eq)

ocamlBasicType :: BasicType -> TypeRep
ocamlBasicType TPtr     = ptr $ con0 "()"
ocamlBasicType TBoolean = con0 "bool"
-- For all the platforms that we support (and those supported by glib)
-- we have gint == gint32. Encoding this assumption in the types saves
-- conversions.
ocamlBasicType TInt     = case sizeOf (0 :: CInt) of
  4 -> con0 "int"
  n -> error ("Unsupported `gint' length: " ++ show n)
ocamlBasicType TUInt = case sizeOf (0 :: CUInt) of
  4 -> con0 "int"
  n -> error ("Unsupported `guint' length: " ++ show n)
ocamlBasicType TLong     = con0 "int"
ocamlBasicType TULong    = con0 "int"
ocamlBasicType TInt8     = con0 "int"
ocamlBasicType TUInt8    = con0 "int"
ocamlBasicType TInt16    = con0 "int"
ocamlBasicType TUInt16   = con0 "int"
ocamlBasicType TInt32    = con0 "int"
ocamlBasicType TUInt32   = con0 "int"
ocamlBasicType TInt64    = con0 "int"
ocamlBasicType TUInt64   = con0 "int"
ocamlBasicType TGType    = con0 "GType"
ocamlBasicType TUTF8     = con0 "string"
ocamlBasicType TFloat    = con0 "float"
ocamlBasicType TDouble   = con0 "float"
ocamlBasicType TUniChar  = con0 "char"
ocamlBasicType TFileName = con0 "string"
ocamlBasicType TIntPtr   = undefined
ocamlBasicType TUIntPtr  = undefined

-- | This translates GI types to the types used for generated OCaml code.
haskellType :: Type -> CodeGen TypeRep
haskellType (TBasicType bt) = return $ ocamlBasicType bt
haskellType (TCArray _ _ _ (TBasicType TUInt8)) =
  return $ "ByteString" `con` []
haskellType (TCArray _ _ _ a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGArray a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TPtrArray a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TByteArray) = return $ "ByteString" `con` []
haskellType (TGList a  ) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGSList a) = do
  inner <- haskellType a
  return $ "[]" `con` [inner]
haskellType (TGHash a b) = do
  innerA <- haskellType a
  innerB <- haskellType b
  return $ "Map.Map" `con` [innerA, innerB]
haskellType TError        = return $ "GError" `con` []
haskellType TVariant      = return $ "GVariant" `con` []
haskellType TParamSpec    = return $ "GParamSpec" `con` []
haskellType (TGClosure _) = do
  tyvar <- getFreshTypeVariable
  return $ "GClosure" `con` [con0 tyvar]
haskellType (TInterface (Name "GObject" "Value")) =
  return $ "Gobject.g_value" `con` []
haskellType t@(TInterface n) = do
  let ocamlName = camelCaseToSnakeCase $ name n
      tname     = lowerName n
  api <- getAPI t
  case api of
    APIFlags _f -> do
      flagsRes <- enumResolver n
      return $ list $ (flagsRes <> "." <> ocamlName) `con` []
    APIEnum _e -> do
      enumRes <- enumResolver n
      return $ (enumRes <> "." <> ocamlName) `con` []
    APIObject    _o -> handleObj ocamlName
    APIInterface _i -> handleObj ocamlName
    APIStruct    _s -> do
      currModule <- currentModule
      currNS     <- currentNS
      let currModuleName = last $ T.splitOn "." currModule

      return $ case (currModuleName == name n, currNS == namespace n) of
        (True , _   ) -> "t" `con` []
        (False, True) -> (name n <> ".t") `con` []
        (False, False) ->
          ("GI" <> namespace n <> "." <> name n <> ".t") `con` []
    APIConst    _c -> return $ "const" `con` []
    APIFunction _f -> return $ "function" `con` []
    APICallback _c -> return $ "callback" `con` []
    APIUnion    _u -> return $ "union" `con` []
 where
  handleObj ocamlName = do
    freshVar <- getFreshTypeVariable
    currMod  <- currentModule
    currNS   <- currentNS
    let currModuleName = last $ T.splitOn "." currMod
        typeVarCon     = typevar freshVar
        ocamlName      = camelCaseToSnakeCase $ name n
        typeRep = case (currNS == namespace n, currModuleName == name n) of
          (True , True ) -> classCon ocamlName
          (True , False) -> classCon $ name n <> "G." <> ocamlName
          (False, _    ) -> ("`" <> ocamlName) `con` []
    return $ obj $ typeVarCon $ polyMore typeRep

enumResolver :: Name -> CodeGen Text
enumResolver n = do
  currNS <- currentNS
  return $ if namespace n == currNS
    then "Enums"
    else "GI" <> namespace n <> "." <> "Enums"

-- | Whether the callable has closure arguments (i.e. "user_data"
-- style arguments).
callableHasClosures :: Callable -> Bool
callableHasClosures = any (/= -1) . map argClosure . args

-- | Check whether the given type corresponds to a callback.
typeIsCallback :: Type -> CodeGen Bool
typeIsCallback t@(TInterface _) = do
  api <- findAPI t
  case api of
    Just (APICallback _) -> return True
    _                    -> return False
typeIsCallback _ = return False

-- | Whether the give type corresponds to an enum or flag.
typeIsEnumOrFlag :: Type -> CodeGen Bool
typeIsEnumOrFlag t = do
  a <- findAPI t
  case a of
    Nothing             -> return False
    (Just (APIEnum  _)) -> return True
    (Just (APIFlags _)) -> return True
    _                   -> return False

-- | Information on how to allocate a type.
data TypeAllocInfo = TypeAllocInfo {
      typeAllocInfoIsBoxed :: Bool
    , typeAllocInfoSize    :: Int -- ^ In bytes.
    }

-- | Information on how to allocate the given type, if known.
typeAllocInfo :: Type -> CodeGen (Maybe TypeAllocInfo)
typeAllocInfo t = do
  api <- findAPI t
  case api of
    Just (APIStruct s) -> case structSize s of
      0 -> return Nothing
      n ->
        let info = TypeAllocInfo { typeAllocInfoIsBoxed = structIsBoxed s
                                 , typeAllocInfoSize    = n
                                 }
        in  return (Just info)
    _ -> return Nothing

-- | This translates GI types to the types used for generated OCaml code.
outParamOcamlType :: Type -> ExcCodeGen TypeRep
outParamOcamlType (TBasicType bt) = return $ ocamlBasicType bt
outParamOcamlType (TCArray _ _ _ (TBasicType TUInt8)) =
  return $ "ByteString" `con` []
outParamOcamlType (TCArray _ _ _ a) = do
  inner <- outParamOcamlType a
  return $ "[]" `con` [inner]
outParamOcamlType (TGArray a) = do
  inner <- outParamOcamlType a
  return $ "[]" `con` [inner]
outParamOcamlType (TPtrArray a) = do
  inner <- outParamOcamlType a
  return $ "[]" `con` [inner]
outParamOcamlType (TByteArray) = return $ "ByteString" `con` []
outParamOcamlType (TGList a  ) = do
  inner <- outParamOcamlType a
  return $ "[]" `con` [inner]
outParamOcamlType (TGSList a) = do
  inner <- outParamOcamlType a
  return $ "[]" `con` [inner]
outParamOcamlType (TGHash a b) = do
  innerA <- outParamOcamlType a
  innerB <- outParamOcamlType b
  return $ "Map.Map" `con` [innerA, innerB]
outParamOcamlType TError        = return $ "GError" `con` []
outParamOcamlType TVariant      = return $ "GVariant" `con` []
outParamOcamlType TParamSpec    = return $ "GParamSpec" `con` []
outParamOcamlType (TGClosure _) = do
  tyvar <- getFreshTypeVariable
  return $ "GClosure" `con` [con0 tyvar]
outParamOcamlType (TInterface (Name "GObject" "Value")) =
  return $ "Gobject.g_value" `con` []
outParamOcamlType t@(TInterface n) = do
  let ocamlName = camelCaseToSnakeCase $ name n
      tname     = lowerName n
  api <- getAPI t
  case api of
    APIFlags _     -> return $ "[]" `con` [tname `con` []]
    APIEnum  _enum -> do
      enumRes <- enumResolver n
      return $ (enumRes <> "." <> ocamlName) `con` []
    APIInterface _ -> handleObj n
    APIObject    _ -> handleObj n
    _              -> return $ con0 "error"
 where
  handleObj n = do
    freshVar <- getFreshTypeVariable
    currNs   <- currentNS
    currMod  <- currentModule
    let
      typeVarCon     = typevar freshVar
      currModuleName = last $ T.splitOn "." currMod
      namespacedType =
        case (namespace n == currNs, name n == currModuleName) of
          (True , True ) -> "t"
          (True , False) -> name n <> ".t"
          (False, _    ) -> "GI" <> namespace n <> "." <> name n <> ".t"
      variant = polyLess $ namespacedType `con` []
      object  = if namespace n == currNs -- TODO: remove if we can construct a class from another lib
        then obj variant
        else obj $ typeVarCon variant
    return object

cType :: Type -> ExcCodeGen Text
cType (TBasicType t) = case t of
  TBoolean  -> return "gboolean"
  TInt      -> return "gint"
  TUInt     -> return "guint"
  TLong     -> return "glong"
  TULong    -> return "gulong"
  TInt8     -> return "gint8"
  TUInt8    -> return "guint8"
  TInt16    -> return "gint16"
  TUInt16   -> return "guint16"
  TInt32    -> return "gint32"
  TUInt32   -> return "guint32"
  TInt64    -> return "gint64"
  TUInt64   -> return "guint64"
  TFloat    -> return "gfloat"
  TDouble   -> return "gdouble"
  TUniChar  -> return "gchar"
  TGType    -> notImplementedError "This cType (TGType) isn't implemented yet"
  TUTF8     -> return "gchar*"
  TFileName -> return "gchar*"
  TPtr      -> return "gpointer"
  TIntPtr   -> return "gintptr"
  TUIntPtr  -> return "guintptr"
cType (TError) =
  notImplementedError "This cType (TError) isn't implemented yet"
cType (TVariant) =
  notImplementedError "This cType (TVariant) isn't implemented yet"
cType (TParamSpec) =
  notImplementedError "This cType (TParamSpec) isn't implemented yet"
cType (TCArray _b _i1 _i2 _t) =
  notImplementedError "This cType (TCArray) isn't implemented yet"
cType (TGArray _t) =
  notImplementedError "This cType (TGArray) isn't implemented yet"
cType (TPtrArray _t) =
  notImplementedError "This cType (TPtrArray) isn't implemented yet"
cType (TByteArray) =
  notImplementedError "This cType (TByteArray) isn't implemented yet"
cType (TGList _t) =
  notImplementedError "This cType (TGList) isn't implemented yet"
cType (TGSList _t) =
  notImplementedError "This cType (TGSList) isn't implemented yet"
cType (TGHash _t1 _t2) =
  notImplementedError "This cType (TGHash) isn't implemented yet"
cType (TGClosure _m) =
  notImplementedError "This cType (TGClosure) isn't implemented yet"
cType (TInterface n) = return $ namespace n <> name n


-- Type to data_conv
ocamlDataConv
  :: Bool             -- ^ is nullable
  -> Type
  -> ExcCodeGen Text
ocamlDataConv _ (TBasicType t) = case t of
  TBoolean -> return "boolean"
  TInt     -> return "int"
  TUInt    -> return "uint"
  TLong    -> return "long"
  TULong   -> return "ulong"
  TInt8    -> notImplementedError "This ocamlDataConv (TInt8) isn't implemented"
  TUInt8 -> notImplementedError "This ocamlDataConv (TUInt8) isn't implemented"
  TInt16 -> notImplementedError "This ocamlDataConv (TInt16) isn't implemented"
  TUInt16 ->
    notImplementedError "This ocamlDataConv (TUInt16) isn't implemented"
  TInt32   -> return "int32"
  TUInt32  -> return "uint32"
  TInt64   -> return "int64"
  TUInt64  -> return "uint64"
  TFloat   -> return "float"
  TDouble  -> return "double"
  TUniChar -> return "char"
  TGType ->
    notImplementedError "This ocamlDataConv (TGType) isn't implemented yet"
  TUTF8     -> return "string"
  TFileName -> return "string"
  TPtr      -> do
    traceShowM "Warning: ocamlDataConv has defaulted a TPtr to int"
    return "int"
  TIntPtr ->
    notImplementedError "This ocamlDataConv (TIntPtr) isn't implemented yet"
  TUIntPtr ->
    notImplementedError "This ocamlDataConv (TUIntPtr) isn't implemented yet"
ocamlDataConv _ (TError) =
  notImplementedError "This ocamlDataConv (TError) isn't implemented yet"
ocamlDataConv _ (TVariant) =
  notImplementedError "This ocamlDataConv (TVariant) isn't implemented yet"
ocamlDataConv _ (TParamSpec) =
  notImplementedError "This ocamlDataConv (TParamSpec) isn't implemented yet"
ocamlDataConv _ (TCArray _b _i1 _i2 _t) =
  notImplementedError "This ocamlDataConv (TCArray) isn't implemented yet"
ocamlDataConv _ (TGArray _t) =
  notImplementedError "This ocamlDataConv (TGArray) isn't implemented yet"
ocamlDataConv _ (TPtrArray _t) =
  notImplementedError "This ocamlDataConv (TPtrArray) isn't implemented yet"
ocamlDataConv _ (TByteArray) =
  notImplementedError "This ocamlDataConv (TByteArray) isn't implemented yet"
ocamlDataConv _ (TGList _t) =
  notImplementedError "This ocamlDataConv (TGList) isn't implemented yet"
ocamlDataConv _ (TGSList _t) =
  notImplementedError "This ocamlDataConv (TGSList) isn't implemented yet"
ocamlDataConv _ (TGHash _t1 _t2) =
  notImplementedError "This ocamlDataConv (TGHash) isn't implemented yet"
ocamlDataConv _ (TGClosure _m) =
  notImplementedError "This ocamlDataConv (TGClosure) isn't implemented yet"
ocamlDataConv isNullable (TInterface n) = do
  api <- findAPIByName n
  case api of
    APIConst _c ->
      notImplementedError "This ocamlDataConv (APIConst) isn't implemented yet"
    APIFunction _f -> notImplementedError
      "This ocamlDataConv (APIFunction) isn't implemented yet"
    APICallback _c -> notImplementedError
      "This ocamlDataConv (APICallback) isn't implemented yet"
    APIEnum      _enum -> enumFlagConv n
    APIFlags     _f    -> enumFlagConv n
    APIInterface _i    -> notImplementedError
      "This ocamlDataConv (APIInterface) isn't implemented yet"
    APIObject _o -> do
      currMod <- currentModule
      currNs  <- currentNS
      if namespace n == currNs
        then do
          let currModuleName = last $ T.splitOn "." currMod
          return $ if name n == currModuleName
            then converter "t"
            else converter (name n <> ".t")
        else do
          let nspace = case namespace n of
                "Pixbuf" -> "GdkPixbuf"  -- TODO: this is kinda hardcoded until we can generate Pixbuf
                nspace   -> nspace
          return $ converter (nspace <> "." <> camelCaseToSnakeCase (name n))

     where
      converter' False conv = "(gobject : " <> conv <> " obj data_conv)"
      converter' True conv =
        "(gobject_option : " <> conv <> " obj option data_conv)"
      converter = converter' isNullable
    APIStruct s -> case structCType s of
      Just t -> if "GdkEvent" `T.isPrefixOf` t
        then do
          let eventType = last $ splitCamelCase t
          return $ "(unsafe_pointer : GdkEvent." <> eventType <> ".t data_conv)"
        else notImplementedError
          "This ocamlDataConv (APIStruct) isn't implemented yet"
      Nothing -> notImplementedError
        "This ocamlDataConv (APIStruct) isn't implemented yet"
    APIUnion _u ->
      notImplementedError "This ocamlDataConv (APIUnion) isn't implemented yet"
 where
  enumFlagConv n = do
    enumRes <- enumResolver n
    return $ enumRes <> "." <> camelCaseToSnakeCase (name n)
          -- return $ T.toTitle (namespace n) <> "Enums.Conv." <> ocamlName

-- Converter from value to C
ocamlValueToC :: Type -> ExcCodeGen Text
ocamlValueToC (TBasicType t) = case t of
  TBoolean -> return "Bool_val"
  TInt     -> return "Int_val"
  TUInt    -> return "Int_val"
  TLong    -> return "Long_val"
  TULong   -> return "Long_val"
  TInt8 ->
    notImplementedError "This ocamlValueToC (TInt8) isn't implemented yet"
  TUInt8 ->
    notImplementedError "This ocamlValueToC (TUInt8) isn't implemented yet"
  TInt16 ->
    notImplementedError "This ocamlValueToC (TInt16) isn't implemented yet"
  TUInt16 ->
    notImplementedError "This ocamlValueToC (TUInt16) isn't implemented yet"
  TInt32 ->
    notImplementedError "This ocamlValueToC (TInt32) isn't implemented yet"
  TUInt32 ->
    notImplementedError "This ocamlValueToC (TUInt32) isn't implemented yet"
  TInt64 ->
    notImplementedError "This ocamlValueToC (TInt64) isn't implemented yet"
  TUInt64 ->
    notImplementedError "This ocamlValueToC (TUInt64) isn't implemented yet"
  TFloat   -> return "Float_val"
  TDouble  -> return "Double_val"
  TUniChar -> return "Char_val"
  TGType ->
    notImplementedError "This ocamlValueToC (TGType) isn't implemented yet"
  TUTF8 -> return "String_val"
  TFileName -> return "String_val"
  TPtr -> notImplementedError "This ocamlValueToC (TPtr) isn't implemented yet"
  TIntPtr ->
    notImplementedError "This ocamlValueToC (TIntPtr) isn't implemented yet"
  TUIntPtr ->
    notImplementedError "This ocamlValueToC (TUIntPtr) isn't implemented yet"
ocamlValueToC TError =
  notImplementedError "This ocamlValueToC (TError) isn't implemented yet"
ocamlValueToC TVariant =
  notImplementedError "This ocamlValueToC (TVariant) isn't implemented yet"
ocamlValueToC TParamSpec =
  notImplementedError "This ocamlValueToC (TParamSpec) isn't implemented yet"
ocamlValueToC (TCArray _b _i1 _i2 _t) =
  notImplementedError "This ocamlValueToC (TCArray) isn't implemented yet"
ocamlValueToC (TGArray _t) =
  notImplementedError "This ocamlValueToC (TGArray) isn't implemented yet"
ocamlValueToC (TPtrArray _t) =
  notImplementedError "This ocamlValueToC (TPtrArray) isn't implemented yet"
ocamlValueToC TByteArray =
  notImplementedError "This ocamlValueToC (TByteArray) isn't implemented yet"
ocamlValueToC (TGList _t) =
  notImplementedError "This ocamlValueToC (TGList) isn't implemented yet"
ocamlValueToC (TGSList _t) =
  notImplementedError "This ocamlValueToC (TGSList) isn't implemented yet"
ocamlValueToC (TGHash _t1 _t2) =
  notImplementedError "This ocamlValueToC (TGHash) isn't implemented yet"
ocamlValueToC (TGClosure _m) =
  notImplementedError "This ocamlValueToC (TGClosure) isn't implemented yet"
ocamlValueToC (TInterface n) = do
  api <- findAPIByName n
  case api of
    APIConst _c ->
      notImplementedError "This ocamlValueToC (APIConst) isn't implemented yet"
    APIFunction _f -> notImplementedError
      "This ocamlValueToC (APIFunction) isn't implemented yet"
    APICallback _c -> notImplementedError
      "This ocamlValueToC (APICallback) isn't implemented yet"
    APIEnum _enum -> do
      addCDep $ namespace n <> "Enums"
      return $ enumVal n
    APIFlags _f -> do
      addCDep $ namespace n <> "Enums"
      return $ flagsVal n
    APIInterface Interface { ifCType = Just ctype } -> converter ctype
    APIInterface _ ->
      notImplementedError "This ocamlValueToC (APIInterface) has no ctype"
    APIObject Object { objCType = Just ctype } -> converter ctype
    APIObject _ ->
      notImplementedError "This ocamlValueToC (APIObject) has no ctype"
    APIStruct Struct { structCType = Just ctype } -> converter ctype
    APIStruct _ ->
      notImplementedError "This ocamlValueToC (APIStruct) has no ctype"
    APIUnion _u ->
      notImplementedError "This ocamlValueToC (APIUnion) isn't implemented yet"
 where
  converter typename = do
    currNS <- currentNS
    addCDep (namespace n <> name n)
    return $ typename <> "_val"

-- Converter from C to value
cToOCamlValue
  :: Bool               -- ^ is nullable
  -> Maybe Type
  -> ExcCodeGen Text
cToOCamlValue _     Nothing               = return "Unit"
cToOCamlValue False (Just (TBasicType t)) = case t of
  TBoolean -> return "Val_bool"
  TInt     -> return "Val_int"
  TUInt    -> return "Val_int"
  TLong    -> return "Val_long"
  TULong   -> return "Val_long"
  TInt8 ->
    notImplementedError "This cToOCamlValue (TInt8) isn't implemented yet"
  TUInt8 ->
    notImplementedError "This cToOCamlValue (TUInt8) isn't implemented yet"
  TInt16 ->
    notImplementedError "This cToOCamlValue (TInt16) isn't implemented yet"
  TUInt16 ->
    notImplementedError "This cToOCamlValue (TUInt16) isn't implemented yet"
  TInt32   -> return "caml_copy_int32"
  TUInt32  -> return "caml_copy_int32"
  TInt64   -> return "caml_copy_int64"
  TUInt64  -> return "caml_copy_int64"
  TFloat   -> return "caml_copy_double"
  TDouble  -> return "caml_copy_double"
  TUniChar -> return "Val_char"
  TGType ->
    notImplementedError "This cToOCamlValue (TGType) isn't implemented yet"
  TUTF8 -> return "Val_string"
  TFileName -> return "Val_string"
  TPtr -> notImplementedError "This cToOCamlValue (TPtr) isn't implemented yet"
  TIntPtr ->
    notImplementedError "This cToOCamlValue (TIntPtr) isn't implemented yet"
  TUIntPtr ->
    notImplementedError "This cToOCamlValue (TUIntPtr) isn't implemented yet"
cToOCamlValue False (Just (TError)) =
  notImplementedError "This cToOCamlValue (TError) isn't implemented yet"
cToOCamlValue False (Just (TVariant)) =
  notImplementedError "This cToOCamlValue (TVariant) isn't implemented yet"
cToOCamlValue False (Just (TParamSpec)) =
  notImplementedError "This cToOCamlValue (TParamSpec) isn't implemented yet"
cToOCamlValue False (Just (TCArray _b _i1 _i2 _t)) =
  notImplementedError "This cToOCamlValue (TCArray) isn't implemented yet"
cToOCamlValue False (Just (TGArray _t)) =
  notImplementedError "This cToOCamlValue (TGArray) isn't implemented yet"
cToOCamlValue False (Just (TPtrArray _t)) =
  notImplementedError "This cToOCamlValue (TPtrArray) isn't implemented yet"
cToOCamlValue False (Just (TByteArray)) =
  notImplementedError "This cToOCamlValue (TByteArray) isn't implemented yet"
cToOCamlValue False (Just (TGList _t)) =
  notImplementedError "This cToOCamlValue (TGList) isn't implemented yet"
cToOCamlValue False (Just (TGSList _t)) =
  notImplementedError "This cToOCamlValue (TGSList) isn't implemented yet"
cToOCamlValue False (Just (TGHash _t1 _t2)) =
  notImplementedError "This cToOCamlValue (TGHash) isn't implemented yet"
cToOCamlValue False (Just (TGClosure _m)) =
  notImplementedError "This cToOCamlValue (TGClosure) isn't implemented yet"
cToOCamlValue False (Just (TInterface n)) = do
  api <- findAPIByName n
  case api of
    APIConst _c ->
      notImplementedError "This cToOCamlValue (APIConst) isn't implemented yet"
    APIFunction _f -> notImplementedError
      "This cToOCamlValue (APIFunction) isn't implemented yet"
    APICallback _c -> notImplementedError
      "This cToOCamlValue (APICallback) isn't implemented yet"
    APIEnum _enum -> return $ valEnum n
    APIFlags _f ->
      notImplementedError "This cToOCamlValue (APIFlags) isn't implemented yet"
    APIInterface _i -> notImplementedError
      "This cToOCamlValue (APIInterface) isn't implemented yet"
    APIObject o -> do
      addCDep (namespace n <> name n)
      return $ "Val_" <> objTypeName o
    APIStruct _s -> notImplementedError
      "This cToOCamlValue (APIStruct) isn't implemented yet"
    APIUnion _u ->
      notImplementedError "This cToOCamlValue (APIUnion) isn't implemented yet"
cToOCamlValue True (Just (TBasicType t)) = case t of
  TUTF8 -> return "Val_option_string"
  TFileName -> return "Val_option_string"
  TPtr -> notImplementedError "This cToOCamlValue (TPtr) isn't implemented yet"
  TIntPtr ->
    notImplementedError "This cToOCamlValue (TIntPtr) isn't implemented yet"
  TUIntPtr ->
    notImplementedError "This cToOCamlValue (TUIntPtr) isn't implemented yet"
  _ ->
    notImplementedError
      "This cToOCamlValue (BasicType) isn't implemented because this type should not be nullable"
cToOCamlValue True (Just (TError)) =
  notImplementedError "This cToOCamlValue (TError) isn't implemented yet"
cToOCamlValue True (Just (TVariant)) =
  notImplementedError "This cToOCamlValue (TVariant) isn't implemented yet"
cToOCamlValue True (Just (TParamSpec)) =
  notImplementedError "This cToOCamlValue (TParamSpec) isn't implemented yet"
cToOCamlValue True (Just (TCArray _b _i1 _i2 _t)) =
  notImplementedError "This cToOCamlValue (TCArray) isn't implemented yet"
cToOCamlValue True (Just (TGArray _t)) =
  notImplementedError "This cToOCamlValue (TGArray) isn't implemented yet"
cToOCamlValue True (Just (TPtrArray _t)) =
  notImplementedError "This cToOCamlValue (TPtrArray) isn't implemented yet"
cToOCamlValue True (Just (TByteArray)) =
  notImplementedError "This cToOCamlValue (TByteArray) isn't implemented yet"
cToOCamlValue True (Just (TGList _t)) =
  notImplementedError "This cToOCamlValue (TGList) isn't implemented yet"
cToOCamlValue True (Just (TGSList _t)) =
  notImplementedError "This cToOCamlValue (TGSList) isn't implemented yet"
cToOCamlValue True (Just (TGHash _t1 _t2)) =
  notImplementedError "This cToOCamlValue (TGHash) isn't implemented yet"
cToOCamlValue True (Just (TGClosure _m)) =
  notImplementedError "This cToOCamlValue (TGClosure) isn't implemented yet"
cToOCamlValue True (Just (TInterface n)) = do
  api <- findAPIByName n
  case api of
    APIConst _c ->
      notImplementedError "This cToOCamlValue (APIConst) isn't implemented yet"
    APIFunction _f -> notImplementedError
      "This cToOCamlValue (APIFunction) isn't implemented yet"
    APICallback _c -> notImplementedError
      "This cToOCamlValue (APICallback) isn't implemented yet"
    APIEnum _enum ->
      notImplementedError "This cToOCamlValue (Enum) isn't implemented yet"
    APIFlags _f ->
      notImplementedError "This cToOCamlValue (APIFlags) isn't implemented yet"
    APIInterface _i -> notImplementedError
      "This cToOCamlValue (APIInterface) isn't implemented yet"
    APIObject o -> do
      addCDep (namespace n <> name n)
      currMod <- currentModule
      unique  <- getFreshTypeVariable
      let currModuleName = last $ T.splitOn "." currMod
          macroName = objTypeName o <> "_" <> currModuleName <> "_" <> unique
      cline $ "Make_Val_option2(" <> objTypeName o <> ", " <> macroName <> ")"
      return $ "Val_option_" <> macroName
    APIStruct _s -> notImplementedError
      "This cToOCamlValue (APIStruct) isn't implemented yet"
    APIUnion _u ->
      notImplementedError "This cToOCamlValue (APIUnion) isn't implemented yet"
