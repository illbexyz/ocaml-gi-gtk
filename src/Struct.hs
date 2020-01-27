-- | Marshalling of structs and unions.
module Struct
  ( genZeroStruct
  , genZeroUnion
  , extractCallbacksInStruct
  , fixAPIStructs
  , ignoreStruct
  , genWrappedPtr
  )
where

import           Control.Applicative            ( (<$>) )
import           Control.Monad                  ( forM
                                                , when
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , isJust
                                                )
import           Data.Monoid                    ( (<>) )

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           API
import           Conversions
import           Code
import           Haddock                        ( addSectionDocumentation
                                                , writeHaddock
                                                , RelativeDocPosition
                                                  ( DocBeforeSymbol
                                                  )
                                                )
import           SymbolNaming                   ( upperName
                                                , lowerName
                                                , underscoresToCamelCase
                                                , qualifiedSymbol
                                                )

import           Type
import           Util

-- TODO: ignore private structs

-- | Whether (not) to generate bindings for the given struct.
ignoreStruct :: Name -> Struct -> Bool
ignoreStruct (Name _ name) s =
  (isJust (gtypeStructFor s) || "Private" `T.isSuffixOf` name)
    && (not $ structForceVisible s)

-- | Whether the given type corresponds to an ignored struct.
isIgnoredStructType :: Type -> CodeGen Bool
isIgnoredStructType t = case t of
  TInterface n -> do
    api <- getAPI t
    case api of
      APIStruct s -> return (ignoreStruct n s)
      _           -> return False
  _ -> return False

-- | Canonical name for the type of a callback type embedded in a
-- struct field.
fieldCallbackType :: Text -> Field -> Text
fieldCallbackType structName field =
  structName <> (underscoresToCamelCase . fieldName) field <> "FieldCallback"

-- | Fix the interface names of callback fields in the struct to
-- correspond to the ones that we are going to generate.
fixCallbackStructFields :: Name -> Struct -> Struct
fixCallbackStructFields (Name ns structName) s = s { structFields = fixedFields
                                                   }
 where
  fixedFields :: [Field]
  fixedFields = map fixField (structFields s)

  fixField :: Field -> Field
  fixField field = case fieldCallback field of
    Nothing -> field
    Just _ ->
      let n' = fieldCallbackType structName field
      in  field { fieldType = TInterface (Name ns n') }

-- | Fix the interface names of callback fields in an APIStruct to
-- correspond to the ones that we are going to generate. If something
-- other than an APIStruct is passed in we don't touch it.
fixAPIStructs :: (Name, API) -> (Name, API)
fixAPIStructs (n, APIStruct s) = (n, APIStruct $ fixCallbackStructFields n s)
fixAPIStructs api              = api

-- | Extract the callback types embedded in the fields of structs, and
-- at the same time fix the type of the corresponding fields. Returns
-- the list of APIs associated to this struct, not including the
-- struct itself.
extractCallbacksInStruct :: (Name, API) -> [(Name, API)]
extractCallbacksInStruct (n@(Name ns structName), APIStruct s)
  | ignoreStruct n s = []
  | otherwise        = mapMaybe callbackInField (structFields s)
 where
  callbackInField :: Field -> Maybe (Name, API)
  callbackInField field = do
    callback <- fieldCallback field
    let n' = fieldCallbackType structName field
    return (Name ns n', APICallback callback)
extractCallbacksInStruct _ = []

-- | The name of the type encoding the information for a field in a
-- struct/union.
infoType :: Name -> Field -> CodeGen Text
infoType owner field = do
  let name  = upperName owner
  let fName = (underscoresToCamelCase . fieldName) field
  return $ name <> fName <> "FieldInfo"

-- | Whether a given field is an embedded struct/union.
isEmbedded :: Field -> ExcCodeGen Bool
isEmbedded field = do
  api <- findAPI (fieldType field)
  case api of
    Just (APIStruct _) -> checkEmbedding
    Just (APIUnion  _) -> checkEmbedding
    _                  -> return False
 where
  checkEmbedding :: ExcCodeGen Bool
  checkEmbedding = case fieldIsPointer field of
    Nothing -> badIntroError "Cannot determine whether the field is embedded."
    Just isPtr -> return (not isPtr)

-- | Name for the getter function
fieldGetter :: Name -> Field -> Text
fieldGetter name' field = "get" <> upperName name' <> fName field

-- | Generate documentation for the given getter.
getterDoc :: Name -> Field -> Text
getterDoc n field = T.unlines
  [ "Get the value of the “@" <> fieldName field <> "@” field."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.get' " <> lowerName n <> " #" <> labelName field
  , "@"
  ]

-- | Name for the setter function
fieldSetter :: Name -> Field -> Text
fieldSetter name' field = "set" <> upperName name' <> fName field

-- | Generate documentation for the given setter.
setterDoc :: Name -> Field -> Text
setterDoc n field = T.unlines
  [ "Set the value of the “@" <> fieldName field <> "@” field."
  , "When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to"
  , ""
  , "@"
  , "'Data.GI.Base.Attributes.set' "
  <> lowerName n
  <> " [ #"
  <> labelName field
  <> " 'Data.GI.Base.Attributes.:=' value ]"
  , "@"
  ]

-- | Return whether the given type corresponds to a callback that does
-- not throw exceptions. See [Note: Callables that throw] for the
-- reason why we do not try to wrap callbacks that throw exceptions.
isRegularCallback :: Type -> CodeGen Bool
isRegularCallback t@(TInterface _) = do
  api <- getAPI t
  case api of
    APICallback (Callback { cbCallable = callable }) ->
      return (not $ callableThrows callable)
    _ -> return False
isRegularCallback _ = return False

-- | Haskell name for the field
fName :: Field -> Text
fName = underscoresToCamelCase . fieldName

-- | Label associated to the field.
labelName :: Field -> Text
labelName = lcFirst . fName

-- | Generate a constructor for a zero-filled struct/union of the given
-- type, using the boxed (or GLib, for unboxed types) allocator.
genZeroSU :: Name -> Int -> Bool -> CodeGen ()
genZeroSU n size isBoxed = group $ do
  let name = upperName n
  let builder = "newZero" <> name
      tsize   = tshow size

  writeHaddock DocBeforeSymbol
               ("Construct a `" <> name <> "` struct initialized to zero.")

  line $ builder <> " :: MonadIO m => m " <> name
  line $ builder <> " = liftIO $ " <> if isBoxed
    then "callocBoxedBytes " <> tsize <> " >>= wrapBoxed " <> name
    else "wrappedPtrCalloc >>= wrapPtr " <> name
  exportDecl builder

  blank

  -- Overloaded "new"
  group $ do
    line $ "instance tag ~ 'AttrSet => Constructible " <> name <> " tag where"
    indent $ do
      line $ "new _ attrs = do"
      indent $ do
        line $ "o <- " <> builder
        line $ "GI.Attributes.set o attrs"
        line $ "return o"

-- | Specialization for structs of `genZeroSU`.
genZeroStruct :: Name -> Struct -> CodeGen ()
genZeroStruct n s =
  when
      (  allocCalloc (structAllocationInfo s)
      /= AllocationOp "none"
      && structSize s
      /= 0
      )
    $ genZeroSU n (structSize s) (structIsBoxed s)

-- | Specialization for unions of `genZeroSU`.
genZeroUnion :: Name -> Union -> CodeGen ()
genZeroUnion n u =
  when
      (  allocCalloc (unionAllocationInfo u)
      /= AllocationOp "none"
      && unionSize u
      /= 0
      )
    $ genZeroSU n (unionSize u) (unionIsBoxed u)

-- | Construct a import with the given prefix.
prefixedForeignImport :: Text -> Text -> Text -> CodeGen Text
prefixedForeignImport prefix symbol prototype = group $ do
  line
    $  "foreign import ccall \""
    <> symbol
    <> "\" "
    <> prefix
    <> symbol
    <> " :: "
    <> prototype
  return (prefix <> symbol)

-- | Same as `prefixedForeignImport`, but import a `FunPtr` to the symbol.
prefixedFunPtrImport :: Text -> Text -> Text -> CodeGen Text
prefixedFunPtrImport prefix symbol prototype = group $ do
  line
    $  "foreign import ccall \"&"
    <> symbol
    <> "\" "
    <> prefix
    <> symbol
    <> " :: FunPtr ("
    <> prototype
    <> ")"
  return (prefix <> symbol)

-- | Generate the typeclass with information for how to
-- allocate/deallocate a given type.
genWrappedPtr :: Name -> AllocationInfo -> Int -> CodeGen ()
genWrappedPtr n info size = group $ do
  let name'  = upperName n

  let prefix = \op -> "_" <> name' <> "_" <> op <> "_"

  when (size == 0 && allocFree info == AllocationOpUnknown)
    $ line
    $ "-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?"

  calloc <- case allocCalloc info of
    AllocationOp "none" ->
      return ("error \"calloc not permitted for " <> name' <> "\"")
    AllocationOp op -> prefixedForeignImport (prefix "calloc") op "IO (Ptr a)"
    AllocationOpUnknown -> if size > 0
      then return ("callocBytes " <> tshow size)
      else return "return nullPtr"

  copy <- case allocCopy info of
    AllocationOp op -> do
      copy <- prefixedForeignImport (prefix "copy") op "Ptr a -> IO (Ptr a)"
      return
        ("\\p -> withManagedPtr p (" <> copy <> " >=> wrapPtr " <> name' <> ")")
    AllocationOpUnknown -> if size > 0
      then return
        (  "\\p -> withManagedPtr p (copyBytes "
        <> tshow size
        <> " >=> wrapPtr "
        <> name'
        <> ")"
        )
      else return "return"

  free <- case allocFree info of
    AllocationOp op ->
      ("Just " <>) <$> prefixedFunPtrImport (prefix "free") op "Ptr a -> IO ()"
    AllocationOpUnknown ->
      if size > 0 then return "Just ptr_to_g_free" else return "Nothing"

  line $ "instance WrappedPtr " <> name' <> " where"
  indent $ do
    line $ "wrappedPtrCalloc = " <> calloc
    line $ "wrappedPtrCopy = " <> copy
    line $ "wrappedPtrFree = " <> free

  line $ "instance WrappedPtr " <> name' <> " where"
