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

import           Data.GI.GIR.BasicTypes         ( BasicType(..)
                                                , Name(..)
                                                )

import           API
import           Code
import           GObject
import           Naming
import           QualifiedNaming                ( escapedArgName )
import           TypeRep
import           Util

import           Debug.Trace


-- | Whether to expose closures and the associated destroy notify
-- handlers in the Haskell wrapper.
data ExposeClosures = WithClosures
                    | WithoutClosures
  deriving (Eq)

getModuleType :: Name -> CodeGen Text
getModuleType n = do
  currNs <- currentNS
  return $ nsOCamlType currNs n

enumResolver :: Name -> CodeGen Text
enumResolver n = do
  currNS <- currentNS
  return $ if namespace n == currNS
    then "Enums"
    else "GI" <> namespace n <> "." <> "Enums"

ocamlBasicType :: BasicType -> TypeRep
ocamlBasicType TPtr     = TextCon "()" -- TODO: check this
ocamlBasicType TBoolean = TextCon "bool"
-- For all the platforms that we support (and those supported by glib)
-- we have gint == gint32. Encoding this assumption in the types saves
-- conversions.
ocamlBasicType TInt     = case sizeOf (0 :: CInt) of
  4 -> TextCon "int"
  n -> error ("Unsupported `gint' length: " ++ show n)
ocamlBasicType TUInt = case sizeOf (0 :: CUInt) of
  4 -> TextCon "int"
  n -> error ("Unsupported `guint' length: " ++ show n)
ocamlBasicType TLong     = TextCon "int"
ocamlBasicType TULong    = TextCon "int"
ocamlBasicType TInt8     = TextCon "int"
ocamlBasicType TUInt8    = TextCon "int"
ocamlBasicType TInt16    = TextCon "int"
ocamlBasicType TUInt16   = TextCon "int"
ocamlBasicType TInt32    = TextCon "int"
ocamlBasicType TUInt32   = TextCon "int"
ocamlBasicType TInt64    = TextCon "int"
ocamlBasicType TUInt64   = TextCon "int"
ocamlBasicType TGType    = TextCon "GType"
ocamlBasicType TUTF8     = TextCon "string"
ocamlBasicType TFloat    = TextCon "float"
ocamlBasicType TDouble   = TextCon "float"
ocamlBasicType TUniChar  = TextCon "char"
ocamlBasicType TFileName = TextCon "string"
ocamlBasicType TIntPtr   = TextCon "error"
ocamlBasicType TUIntPtr  = TextCon "error"
-- ocamlBasicType TIntPtr   = error "(ocamlBasicType) can't handle TIntPtr"
-- ocamlBasicType TUIntPtr  = error "(ocamlBasicType) can't handle TUIntPtr"

-- | This translates GI types to the types used for generated OCaml code.
haskellType :: Type -> CodeGen TypeRep
haskellType (TBasicType bt                    ) = return $ ocamlBasicType bt
haskellType (TCArray _ _ _ (TBasicType TUInt8)) = return $ TextCon "ByteString"
haskellType (TCArray _ _ _ a                  ) = ListCon <$> haskellType a
haskellType (TGArray   a                      ) = ListCon <$> haskellType a
haskellType (TPtrArray a                      ) = ListCon <$> haskellType a
haskellType TByteArray                          = return $ TextCon "ByteString"
haskellType (TGList  a )                        = ListCon <$> haskellType a
haskellType (TGSList a )                        = ListCon <$> haskellType a
haskellType (TGHash a b)                        = do
  currNS <- currentNS
  innerA <- typeShow currNS <$> haskellType a
  innerB <- typeShow currNS <$> haskellType b
  return $ TextCon $ "(" <> innerA <> ", " <> innerB <> ") Hashtbl.t"
haskellType TError        = return $ TextCon "GError"
haskellType TVariant      = return $ TextCon "GVariant"
haskellType TParamSpec    = return $ TextCon "GParamSpec"
haskellType (TGClosure _) = do
  tyvar <- getFreshTypeVariable
  -- error "(haskellType) can't handle TGClosure"
  return $ TextCon "error"
haskellType (TInterface (Name "GObject" "Value")) =
  return $ TextCon "Gobject.g_value"
haskellType t@(TInterface n) = do
  let ocamlName = ocamlIdentifier n
      tname     = lowerName n
  api <- getAPI t
  case api of
    APIFlags _f -> do
      flagsRes <- enumResolver n
      return $ ListCon $ TextCon $ (flagsRes <> "." <> ocamlName)
    APIEnum _e -> do
      enumRes <- enumResolver n
      return $ TextCon $ enumRes <> "." <> ocamlName
    APIObject    _o -> handleObj
    APIInterface _i -> handleObj
    APIStruct    _s -> TextCon <$> getModuleType n
    APIConst     _c -> return $ TextCon "const"
    APIFunction  _f -> return $ TextCon "function"
    APICallback  _c -> return $ TextCon "callback"
    APIUnion     _u -> return $ TextCon "union"
 where
  handleObj = do
    freshVar <- getFreshTypeVariable
    return $ ObjCon $ TypeVarCon freshVar $ RowCon More $ PolyCon $ NameCon n

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
outParamOcamlType t@(TCArray _ _ _ (TBasicType TUInt8))   = haskellType t
outParamOcamlType t@(TCArray _ _ _ a                  )   = haskellType t
outParamOcamlType t@(TGArray   a                      )   = haskellType t
outParamOcamlType t@(TPtrArray a                      )   = haskellType t
outParamOcamlType t@TByteArray                            = haskellType t
outParamOcamlType t@(TGList  a )                          = haskellType t
outParamOcamlType t@(TGSList a )                          = haskellType t
outParamOcamlType t@(TGHash a b)                          = haskellType t
outParamOcamlType t@TError                                = haskellType t
outParamOcamlType t@TVariant                              = haskellType t
outParamOcamlType t@TParamSpec                            = haskellType t
outParamOcamlType t@(TGClosure  _                       ) = haskellType t
outParamOcamlType t@(TInterface (Name "GObject" "Value")) = haskellType t
outParamOcamlType t@(TInterface n                       ) = do
  let ocamlName = ocamlIdentifier n
      tname     = lowerName n
  api <- getAPI t
  case api of
    APIFlags     _ -> handleEnum ocamlName
    APIEnum      _ -> handleEnum ocamlName
    APIInterface _ -> handleObj n
    APIObject    _ -> handleObj n
    _ -> notImplementedError "(outParamOcamlType) can't handle this type"
 where
  handleEnum ocamlName = do
    enumRes <- enumResolver n
    return $ TextCon $ enumRes <> "." <> ocamlName
  handleObj n = do
    freshVar <- getFreshTypeVariable
    return $ ObjCon $ TypeVarCon freshVar $ RowCon Less $ PolyCon $ NameCon n

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
cType (TInterface n  )        = do
  api <- findAPIByName n
  case api of
    APIConst     c         -> return $ constantCType c
    APIFunction  f         -> return $ fnSymbol f -- Not sure
    APICallback Callback { cbCType = Just ctype } -> return ctype
    APICallback  _         -> cTypeErr "Callback without cType"
    APIEnum      e         -> return $ enumCType e
    APIFlags     (Flags e) -> return $ enumCType e
    APIInterface Interface { ifCType = Just ctype } -> return ctype
    APIInterface _         -> cTypeErr "Interface without cType"
    APIObject Object { objCType = Just ctype } -> return $ ctype <> "*"
    APIObject    _         -> cTypeErr "Object without cType"
    APIStruct Struct { structCType = Just ctype } -> return ctype
    APIStruct    _         -> cTypeErr "Struct without cType"
    APIUnion Union { unionCType = Just ctype } -> return ctype
    APIUnion     _         -> cTypeErr "Union without cType"

cTypeErr :: Text -> ExcCodeGen Text
cTypeErr = conversionError "cType"

objConverter
  :: Bool     -- ^ is nullable
  -> Text
  -> Text
objConverter False conv = "(gobject : " <> conv <> " Gobject.obj data_conv)"
objConverter True conv =
  "(gobject_option : " <> conv <> " Gobject.obj option data_conv)"

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
    APIUnion _u -> do
      moduleT <- getModuleType n
      return $ "(unsafe_pointer : " <> moduleT <> " data_conv)"
 where
  handleObject = do
    convType <- getModuleType n
    return $ objConverter isNullable convType
  enumFlagConv n = do
    enumRes <- enumResolver n
    return $ enumRes <> "." <> ocamlIdentifier n

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
    APIInterface Interface { ifCType = Just _ } -> apiConverter
    APIInterface _                              -> notImplementedError  -- TODO: probably we don't need the ctype anymore
      "(ocamlValueToC) Can't convert a APIInterface with no ctype"
    APIObject Object { objCType = Just _ } -> apiConverter
    APIObject _                            -> notImplementedError
      "(ocamlValueToC) Can't convert a APIObject with no ctype"
    APIStruct Struct { structCType = Just _ } -> apiConverter
    APIStruct _                               -> notImplementedError
      "(ocamlValueToC) Can't convert a APIStruct with no ctype"
    APIUnion _u -> ocamlValueToCErr "APIUnion"
 where
  apiConverter = case n of
    Name "GObject" "Value" -> return "GValue_val"
    _                      -> do
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
    APIConst    _c    -> cToOCamlValueErr "APIConst"
    APIFunction _f    -> cToOCamlValueErr "APIFunction"
    APICallback _c    -> cToOCamlValueErr "APICallback"
    APIEnum     _enum -> do
      addCDep $ namespace n <> "Enums"
      return $ valEnum n
    APIFlags     _f -> cToOCamlValueErr "APIFlags"
    APIInterface _i -> do
      addCDep (namespace n <> name n)
      return $ valInterface n
    APIObject _o -> do
      addCDep (namespace n <> name n)
      return $ valObject n
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
