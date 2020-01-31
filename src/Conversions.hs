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

getModuleType :: Name -> CodeGen Text
getModuleType n = do
  currMod <- currentModule
  currNs  <- currentNS
  let currModuleName = last $ T.splitOn "." currMod
  return $ case (currNs == namespace n, currModuleName == name n) of
    (True , True ) -> "t"
    (True , False) -> name n <> ".t"
    (False, _    ) -> "GI" <> namespace n <> "." <> name n <> ".t"

enumResolver :: Name -> CodeGen Text
enumResolver n = do
  currNS <- currentNS
  return $ if namespace n == currNS
    then "Enums"
    else "GI" <> namespace n <> "." <> "Enums"

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
      moduleType <- getModuleType n
      return $ moduleType `con` []
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
    APIFlags     _ -> handleEnum ocamlName
    APIEnum      _ -> handleEnum ocamlName
    APIInterface _ -> handleObj n
    APIObject    _ -> handleObj n
    _              -> return $ con0 "error"
 where
  handleEnum ocamlName = do
    enumRes <- enumResolver n
    return $ (enumRes <> "." <> ocamlName) `con` []
  handleObj n = do
    freshVar       <- getFreshTypeVariable
    currNs         <- currentNS
    namespacedType <- getModuleType n
    let typeVarCon = typevar freshVar
        variant    = polyLess $ namespacedType `con` []
        object     = if namespace n == currNs -- TODO: remove if we can construct a class from another lib
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
  TGType    -> cTypeErr "TGType"
  TUTF8     -> return "gchar*"
  TFileName -> return "gchar*"
  TPtr      -> return "gpointer"
  TIntPtr   -> return "gintptr"
  TUIntPtr  -> return "guintptr"
cType TError                  = cTypeErr "TError"
cType TVariant                = cTypeErr "TVariant"
cType TParamSpec              = cTypeErr "TParamSpec"
cType (TCArray _b _i1 _i2 _t) = cTypeErr "TCArray"
cType (TGArray   _t         ) = cTypeErr "TGArray"
cType (TPtrArray _t         ) = cTypeErr "TPtrArray"
cType TByteArray              = cTypeErr "TByteArray"
cType (TGList  _t    )        = cTypeErr "TGList"
cType (TGSList _t    )        = cTypeErr "TGSList"
cType (TGHash _t1 _t2)        = cTypeErr "TGHash"
cType (TGClosure  _m )        = cTypeErr "TGClosure"
cType (TInterface n  )        = return $ namespace n <> name n -- TODO: Probably need to extract the interfaceCType

cTypeErr :: Text -> ExcCodeGen Text
cTypeErr = conversionError "cType"

objConverter
  :: Bool     -- ^ is nullable
  -> Text
  -> Text
objConverter False conv = "(gobject : " <> conv <> " obj data_conv)"
objConverter True conv =
  "(gobject_option : " <> conv <> " obj option data_conv)"

-- Type to data_conv
ocamlDataConv
  :: Bool             -- ^ is nullable
  -> Type
  -> ExcCodeGen Text
ocamlDataConv _ (TBasicType t) = case t of
  TBoolean  -> return "boolean"
  TInt      -> return "int"
  TUInt     -> return "uint"
  TLong     -> return "long"
  TULong    -> return "ulong"
  TInt8     -> ocamlDataConvErr "TInt8"
  TUInt8    -> ocamlDataConvErr "TUInt8"
  TInt16    -> ocamlDataConvErr "TInt16"
  TUInt16   -> ocamlDataConvErr "TUInt16"
  TInt32    -> return "int32"
  TUInt32   -> return "uint32"
  TInt64    -> return "int64"
  TUInt64   -> return "uint64"
  TFloat    -> return "float"
  TDouble   -> return "double"
  TUniChar  -> return "char"
  TGType    -> ocamlDataConvErr "TGType"
  TUTF8     -> return "string"
  TFileName -> return "string"
  TPtr      -> do
    traceShowM "Warning: ocamlDataConv has defaulted a TPtr to int"
    return "int"
  TIntPtr  -> ocamlDataConvErr "TIntPtr"
  TUIntPtr -> ocamlDataConvErr "TUIntPtr"
ocamlDataConv _          TError                  = ocamlDataConvErr "TError"
ocamlDataConv _          TVariant                = ocamlDataConvErr "TVariant"
ocamlDataConv _          TParamSpec              = ocamlDataConvErr "TParamSpec"
ocamlDataConv _          (TCArray _b _i1 _i2 _t) = ocamlDataConvErr "TCArray"
ocamlDataConv _          (TGArray   _t         ) = ocamlDataConvErr "TGArray"
ocamlDataConv _          (TPtrArray _t         ) = ocamlDataConvErr "TPtrArray"
ocamlDataConv _          TByteArray              = ocamlDataConvErr "TByteArray"
ocamlDataConv _          (TGList  _t    )        = ocamlDataConvErr "TGList"
ocamlDataConv _          (TGSList _t    )        = ocamlDataConvErr "TGSList"
ocamlDataConv _          (TGHash _t1 _t2)        = ocamlDataConvErr "TGHash"
ocamlDataConv _          (TGClosure  _m )        = ocamlDataConvErr "TGClosure"
ocamlDataConv isNullable (TInterface n  )        = do
  api <- findAPIByName n
  case api of
    APIConst     _c    -> ocamlDataConvErr "APIConst"
    APIFunction  _f    -> ocamlDataConvErr "APIFunction"
    APICallback  _c    -> ocamlDataConvErr "APICallback"
    APIEnum      _enum -> enumFlagConv n
    APIFlags     _f    -> enumFlagConv n
    APIInterface _i    -> handleObject
    APIObject    _o    -> handleObject
    APIStruct    _s    -> do
      moduleT <- getModuleType n
      return $ "(unsafe_pointer : " <> moduleT <> " data_conv)"
      -- case structCType s of
      --   Just t -> if "GdkEvent" `T.isPrefixOf` t
      --     then do
      --       let eventType = last $ splitCamelCase t
      --       return $ "(unsafe_pointer : GdkEvent." <> eventType <> ".t data_conv)"
      --     else ocamlDataConvErr "APIStruct"
      --   Nothing -> ocamlDataConvErr "APIStruct"
    APIUnion _u -> ocamlDataConvErr "APIUnion"
 where
  handleObject = do
    convType <- getModuleType n
    return $ objConverter isNullable convType
  enumFlagConv n = do
    enumRes <- enumResolver n
    return $ enumRes <> "." <> camelCaseToSnakeCase (name n)

ocamlDataConvErr :: Text -> ExcCodeGen Text
ocamlDataConvErr = conversionError "ocamlDataConv"

-- Converter from value to C
ocamlValueToC :: Type -> ExcCodeGen Text
ocamlValueToC (TBasicType t) = case t of
  TBoolean  -> return "Bool_val"
  TInt      -> return "Int_val"
  TUInt     -> return "Int_val"
  TLong     -> return "Long_val"
  TULong    -> return "Long_val"
  TInt8     -> ocamlValueToCErr "TInt8"
  TUInt8    -> ocamlValueToCErr "TUInt8"
  TInt16    -> ocamlValueToCErr "TInt16"
  TUInt16   -> ocamlValueToCErr "TUInt16"
  TInt32    -> ocamlValueToCErr "TInt32"
  TUInt32   -> ocamlValueToCErr "TUInt32"
  TInt64    -> ocamlValueToCErr "TInt64"
  TUInt64   -> ocamlValueToCErr "TUInt64"
  TFloat    -> return "Float_val"
  TDouble   -> return "Double_val"
  TUniChar  -> return "Char_val"
  TGType    -> ocamlValueToCErr "TGType"
  TUTF8     -> return "String_val"
  TFileName -> return "String_val"
  TPtr      -> ocamlValueToCErr "TPtr"
  TIntPtr   -> ocamlValueToCErr "TIntPtr"
  TUIntPtr  -> ocamlValueToCErr "TUIntPtr"
ocamlValueToC TError                  = ocamlValueToCErr "TError"
ocamlValueToC TVariant                = ocamlValueToCErr "TVariant"
ocamlValueToC TParamSpec              = ocamlValueToCErr "TParamSpec"
ocamlValueToC (TCArray _b _i1 _i2 _t) = ocamlValueToCErr "TCArray"
ocamlValueToC (TGArray   _t         ) = ocamlValueToCErr "TGArray"
ocamlValueToC (TPtrArray _t         ) = ocamlValueToCErr "TPtrArray"
ocamlValueToC TByteArray              = ocamlValueToCErr "TByteArray"
ocamlValueToC (TGList  _t    )        = ocamlValueToCErr "TGList"
ocamlValueToC (TGSList _t    )        = ocamlValueToCErr "TGSList"
ocamlValueToC (TGHash _t1 _t2)        = ocamlValueToCErr "TGHash"
ocamlValueToC (TGClosure  _m )        = ocamlValueToCErr "TGClosure"
ocamlValueToC (TInterface n  )        = do
  api <- findAPIByName n
  case api of
    APIConst    _c    -> ocamlValueToCErr "APIConst"
    APIFunction _f    -> ocamlValueToCErr "APIFunction"
    APICallback _c    -> ocamlValueToCErr "APICallback"
    APIEnum     _enum -> do
      addCDep $ namespace n <> "Enums"
      return $ enumVal n
    APIFlags _f -> do
      addCDep $ namespace n <> "Enums"
      return $ flagsVal n
    APIInterface Interface { ifCType = Just ctype } -> converter ctype
    APIInterface _ -> notImplementedError
      "(ocamlValueToC) Can't convert a APIInterface with no ctype"
    APIObject Object { objCType = Just ctype } -> converter ctype
    APIObject _ -> notImplementedError
      "(ocamlValueToC) Can't convert a APIObject with no ctype"
    APIStruct Struct { structCType = Just ctype } -> converter ctype
    APIStruct _ -> notImplementedError
      "(ocamlValueToC) Can't convert a APIStruct with no ctype"
    APIUnion _u -> ocamlValueToCErr "APIUnion"
 where
  converter typename = do
    currNS <- currentNS
    addCDep (namespace n <> name n)
    return $ interfaceVal n

ocamlValueToCErr :: Text -> ExcCodeGen Text
ocamlValueToCErr = conversionError "ocamlValueToC"

-- Converter from C to value
cToOCamlValue
  :: Bool               -- ^ is nullable
  -> Maybe Type
  -> ExcCodeGen Text
cToOCamlValue _     Nothing               = return "Unit"
cToOCamlValue False (Just (TBasicType t)) = case t of
  TBoolean  -> return "Val_bool"
  TInt      -> return "Val_int"
  TUInt     -> return "Val_int"
  TLong     -> return "Val_long"
  TULong    -> return "Val_long"
  TInt8     -> cToOCamlValueErr "TInt8"
  TUInt8    -> cToOCamlValueErr "TUInt8"
  TInt16    -> cToOCamlValueErr "TInt16"
  TUInt16   -> cToOCamlValueErr "TUInt16"
  TInt32    -> return "caml_copy_int32"
  TUInt32   -> return "caml_copy_int32"
  TInt64    -> return "caml_copy_int64"
  TUInt64   -> return "caml_copy_int64"
  TFloat    -> return "caml_copy_double"
  TDouble   -> return "caml_copy_double"
  TUniChar  -> return "Val_char"
  TGType    -> cToOCamlValueErr "TGType"
  TUTF8     -> return "Val_string"
  TFileName -> return "Val_string"
  TPtr      -> cToOCamlValueErr "TPtr"
  TIntPtr   -> cToOCamlValueErr "TIntPtr"
  TUIntPtr  -> cToOCamlValueErr "TUIntPtr"
cToOCamlValue True (Just (TBasicType t)) = case t of
  TUTF8     -> return "Val_option_string"
  TFileName -> return "Val_option_string"
  TPtr      -> cToOCamlValueErr "TPtr"
  TIntPtr   -> cToOCamlValueErr "TIntPtr"
  TUIntPtr  -> cToOCamlValueErr "TUIntPtr"
  _ ->
    notImplementedError
      "(cToOCamlValue) BasicType isn't implemented because this type should not be nullable"
cToOCamlValue False (Just TError) = cToOCamlValueErr "TError"
cToOCamlValue True (Just TError) = cToOCamlValueErr "TError"
cToOCamlValue False (Just TVariant) = cToOCamlValueErr "TVariant"
cToOCamlValue True (Just TVariant) = cToOCamlValueErr "TVariant"
cToOCamlValue False (Just TParamSpec) = cToOCamlValueErr "TParamSpec"
cToOCamlValue True (Just TParamSpec) = cToOCamlValueErr "TParamSpec"
cToOCamlValue False (Just (TCArray _b _i1 _i2 _t)) = cToOCamlValueErr "TCArray"
cToOCamlValue True (Just (TCArray _b _i1 _i2 _t)) = cToOCamlValueErr "TCArray"
cToOCamlValue False (Just (TGArray _t)) = cToOCamlValueErr "TGArray"
cToOCamlValue True (Just (TGArray _t)) = cToOCamlValueErr "TGArray"
cToOCamlValue False (Just (TPtrArray _t)) = cToOCamlValueErr "TPtrArray"
cToOCamlValue True (Just (TPtrArray _t)) = cToOCamlValueErr "TPtrArray"
cToOCamlValue False (Just TByteArray) = cToOCamlValueErr "TByteArray"
cToOCamlValue True (Just TByteArray) = cToOCamlValueErr "TByteArray"
cToOCamlValue False (Just (TGList _t)) = cToOCamlValueErr "TGList"
cToOCamlValue True (Just (TGList _t)) = cToOCamlValueErr "TGList"
cToOCamlValue False (Just (TGSList _t)) = cToOCamlValueErr "TGSList"
cToOCamlValue True (Just (TGSList _t)) = cToOCamlValueErr "TGSList"
cToOCamlValue False (Just (TGHash _t1 _t2)) = cToOCamlValueErr "TGHash"
cToOCamlValue True (Just (TGHash _t1 _t2)) = cToOCamlValueErr "TGHash"
cToOCamlValue False (Just (TGClosure _m)) = cToOCamlValueErr "TGClosure"
cToOCamlValue True (Just (TGClosure _m)) = cToOCamlValueErr "TGClosure"
cToOCamlValue False (Just (TInterface n)) = do
  api <- findAPIByName n
  case api of
    APIConst     _c    -> cToOCamlValueErr "APIConst"
    APIFunction  _f    -> cToOCamlValueErr "APIFunction"
    APICallback  _c    -> cToOCamlValueErr "APICallback"
    APIEnum      _enum -> return $ valEnum n
    APIFlags     _f    -> cToOCamlValueErr "APIFlags"
    APIInterface _i    -> do
      addCDep (namespace n <> name n)
      return $ valInterface n
    APIObject o -> do
      addCDep (namespace n <> name n)
      return $ "Val_" <> objTypeName o
    APIStruct _s -> cToOCamlValueErr "APIStruct"
    APIUnion  _u -> cToOCamlValueErr "APIUnion"
cToOCamlValue True (Just (TInterface n)) = do
  api <- findAPIByName n
  case api of
    APIConst     _c    -> cToOCamlValueErr "APIConst"
    APIFunction  _f    -> cToOCamlValueErr "APIFunction"
    APICallback  _c    -> cToOCamlValueErr "APICallback"
    APIEnum      _enum -> cToOCamlValueErr "APIEnum"
    APIFlags     _f    -> cToOCamlValueErr "APIFlags"
    APIInterface _i    -> do
      addCDep (namespace n <> name n)
      return $ valOptInterface n
    APIObject o -> do
      addCDep (namespace n <> name n)
      return $ valOptObject n
    APIStruct _s -> cToOCamlValueErr "APIStruct"
    APIUnion  _u -> cToOCamlValueErr "APIUnion"

cToOCamlValueErr :: Text -> ExcCodeGen Text
cToOCamlValueErr = conversionError "cToOcamlValue"

conversionError :: Text -> Text -> ExcCodeGen Text
conversionError converterName converterCase =
  notImplementedError
    $  "This "
    <> converterName
    <> " ("
    <> converterCase
    <> ") isn't implemented yet"
