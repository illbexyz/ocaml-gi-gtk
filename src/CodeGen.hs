module CodeGen
  ( genConstant
  , genFunction
  , genModule
  )
where

import           Control.Monad
import           Data.Maybe                     ( mapMaybe
                                                , isNothing
                                                , fromJust
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

import           API
import           Conversions
import           TypeRep
import           Constant                       ( genConstant )
import           EnumFlags                      ( genEnum
                                                , genFlags
                                                )
import           Fixups                         ( dropMovedItems
                                                , guessPropertyNullability
                                                , detectGObject
                                                , dropDuplicatedFields
                                                , checkClosureDestructors
                                                )
import           Object                         ( genObject
                                                , genInterface
                                                )
import           Method                         ( genMethod )
import           Struct                         ( extractCallbacksInStruct
                                                , fixAPIStructs
                                                , ignoreStruct
                                                )
import           Code
import           Callable                       ( genCCallableWrapper )
import           Naming
import           QualifiedNaming                ( submoduleLocation )
import           Debug.Trace


genFunction :: Name -> Function -> CodeGen ()
genFunction n (Function symbol fnMovedTo callable) =
    -- Only generate the function if it has not been moved.
  when (Nothing == fnMovedTo) $ group $ do
    line $ "-- function " <> symbol
    handleCGExc
      (\e -> line
        (  "(* Could not generate function "
        <> symbol
        <> " *)\n(* Error was : "
        <> describeCGError e
        <> " *)"
        )
      )
      (genCCallableWrapper n symbol callable)

-- TODO: A struct ovverride type
genStructCasts :: Name -> Struct -> CodeGen ()
genStructCasts n s = do
  let mbCType = structCType s
  forM_ mbCType $ \cType -> case cType of
    "GdkAtom" ->
      hline
        $  "#define "
        <> structVal n
        <> "(val) ((GdkAtom) MLPointer_val(val))"
    _ -> do
      hline
        $  "#define "
        <> structVal n
        <> "(val) (("
        <> cType
        <> "*) MLPointer_val(val))"

      hline $ "#define " <> valStruct n <> " Val_pointer"
  addCDep (namespace n <> name n)

-- | Generate wrapper for structures.
genStruct :: Name -> Struct -> CodeGen ()
genStruct n s = unless (ignoreStruct n s) $ do
  let name'     = upperName n
      ocamlName = ocamlIdentifier n
  -- writeHaddock DocBeforeSymbol "Memory-managed wrapper type."

  -- addSectionDocumentation ToplevelSection (structDocumentation s)
  addType n Nothing

  genStructCasts n s

  when (n == Name "Gtk" "PageRange" || n == Name "Gtk" "Border") $ do
  -- traceShowM s
    handleCGExc (\e -> line $ describeCGError e) (genCreate ocamlName)
    group $ forM_ (structFields s) (genField ocamlName)
    group $ forM_ (structMethods s) genStructMethod

 where
  genCreate :: Text -> ExcCodeGen ()
  genCreate ocamlName = do
    currNS     <- currentNS
    fieldTypes <- forM (structFields s)
      $ \f -> typeShow currNS <$> haskellType (fieldType f)
    fieldConvs <- forM (structFields s) $ \f -> ocamlValueToC (fieldType f)
    let mlName     = mlGiPrefix n (ocamlName <> "_create")
        fieldNames = fieldName <$> structFields s
    line
      $  "external create : "
      <> T.intercalate " -> " fieldTypes
      <> " -> "
      <> nsOCamlType currNS n
      <> " = \""
      <> mlName
      <> "\""

    cline
      $  "CAMLprim value "
      <> mlName
      <> "(value "
      <> T.intercalate ", value " fieldNames
      <> ") {"

    ctype <- case structCType s of
      Just typ -> return typ
      Nothing  -> missingInfoError "Need ctype"

    cline $ "  " <> ctype <> " " <> ocamlName <> ";"
    forM_ (zip fieldNames fieldConvs) $ \(fN, conv) ->
      cline
        $  "  "
        <> ocamlName
        <> "."
        <> fN
        <> " = "
        <> conv
        <> "("
        <> fN
        <> ");"
    cline $ "  return Val_copy(" <> ocamlName <> ");"
    cline "}"
    cline ""


  genField :: Text -> Field -> CodeGen ()
  genField ocamlName f = do
    let fName = escapeOCamlReserved $ fieldName f
        fType = fieldType f
    currNS   <- currentNS
    fTypeStr <- typeShow currNS <$> haskellType fType
    handleCGExc (\e -> line $ "(* " <> describeCGError e <> " *)")
                (genFieldExtractor f)
    line
      $  "external "
      <> fName
      <> " : "
      <> nsOCamlType currNS n
      <> " -> "
      <> fTypeStr
      <> " = \""
      <> mlGiPrefix n (ocamlName <> "_" <> fieldName f)
      <> "\""

  genFieldExtractor :: Field -> ExcCodeGen ()
  genFieldExtractor f = do
    let nspace     = T.toLower (namespace n)
        structN    = camelCaseToSnakeCase (name n)
        structConv = structVal n
        fieldN     = fieldName f
    fieldConv <- cToOCamlValue False (Just . fieldType $ f)
    cline
      $  "Make_Extractor ("
      <> T.intercalate ", " [nspace, structN, structConv, fieldN, fieldConv]
      <> ")"

  genStructMethod :: Method -> CodeGen ()
  genStructMethod m@Method { methodName = mn, methodSymbol = sym, methodCallable = c, methodType = t }
    = do
      isFunction <- symbolFromFunction (methodSymbol m)
      unless isFunction $ handleCGExc
        (\e -> line
          (  "(* Could not generate method "
          <> name mn
          <> " *)\n"
          <> "(* Error was : "
          <> describeCGError e
          <> " *)"
          )
        )
        (genCCallableWrapper mn sym c)
  -- if structIsBoxed s
  --   then traceShowM $ "Struct " <> show n <> " is boxed"
  --   else traceShowM $ "Struct " <> show n <> " not boxed"

  -- if structIsBoxed s
  --   then genBoxedObject n (fromJust $ structTypeInit s)
  --   else genWrappedPtr n (structAllocationInfo s) (structSize s)

-- TODO: A struct ovverride type
genUnionCasts :: Name -> Union -> CodeGen ()
genUnionCasts n u = do
  let mbCType = unionCType u
  forM_ mbCType $ \cType ->
    hline
      $  "#define "
      <> structVal n
      <> "(val) (("
      <> cType
      <> "*) MLPointer_val(val))"
      -- TODO: Val_

-- | Generated wrapper for unions.
genUnion :: Name -> Union -> CodeGen ()
genUnion n u = do
  -- let name' = upperName n

  -- writeHaddock DocBeforeSymbol "Memory-managed wrapper type."

  -- addSectionDocumentation ToplevelSection (unionDocumentation u)

  addType n Nothing

  genUnionCasts n u

  -- if unionIsBoxed u
  --   then genBoxedObject n (fromJust $ unionTypeInit u)
  --   else genWrappedPtr n (unionAllocationInfo u) (unionSize u)

  -- Generate code for fields.
  -- genStructOrUnionFields n (unionFields u)

  -- Methods
  -- _methods <- forM (unionMethods u) $ \f -> do
  --   let mn = methodName f
  --   isFunction <- symbolFromFunction (methodSymbol f)
  --   if not isFunction
  --     then handleCGExc
  --       (\e ->
  --         line
  --             (  "(* Could not generate method "
  --             <> name'
  --             <> "::"
  --             <> name mn
  --             <> " *)\n"
  --             <> "(* Error was : "
  --             <> describeCGError e
  --             <> " *)"
  --             )
  --           >> return Nothing
  --       )
  --       (genMethod n f >> return (Just (n, f)))
  --     else return Nothing

  -- return ()

-- Some type libraries include spurious interface/struct methods,
-- where a method Mod.Foo::func also appears as an ordinary function
-- in the list of APIs. If we find a matching function (without the
-- "moved-to" annotation), we don't generate the method.
--
-- It may be more expedient to keep a map of symbol -> function.
symbolFromFunction :: Text -> CodeGen Bool
symbolFromFunction sym = any (hasSymbol sym . snd) . M.toList <$> getAPIs
 where
  hasSymbol sym1 (APIFunction Function { fnSymbol = sym2, fnMovedTo = movedTo })
    = sym1 == sym2 && isNothing movedTo
  hasSymbol _ _ = False

-- genAPI :: Name -> API -> CodeGen ()
-- genAPI n (APIConst c) = genConstant n c
-- genAPI n (APIFunction f) = genFunction n f
-- genAPI n (APIEnum e) = genEnum n e
-- genAPI n (APIFlags f) = genFlags n f
-- genAPI n (APICallback c) = genCallback n c
-- genAPI n (APIStruct s) = genStruct n s
-- genAPI n (APIUnion u) = genUnion n u
-- genAPI n (APIObject o) = genObject n o
-- genAPI n (APIInterface i) = genInterface n i

genAPI :: Name -> API -> CodeGen ()
genAPI _n (APIConst     _c) = return ()
genAPI _n (APIFunction  _f) = return ()
genAPI n  (APIEnum      e ) = genEnum n e
genAPI n  (APIFlags     f ) = genFlags n f
genAPI _n (APICallback  _c) = return ()
genAPI n  (APIStruct    s ) = genStruct n s
genAPI n  (APIUnion     u ) = genUnion n u
genAPI n  (APIObject    o ) = genObject n o
genAPI n  (APIInterface i ) = genInterface n i

-- | Generate the code for a given API in the corresponding module.
genAPIModule :: Name -> API -> CodeGen ()
genAPIModule n api = submodule (submoduleLocation n api) $ genAPI n api

genModule' :: M.Map Name API -> CodeGen ()
genModule' apis = do
  mapM_ (uncurry genAPIModule)
            -- We provide these ourselves
    $ filter
        ( (`notElem` [ Name "GLib"    "Array"
                     , Name "GLib"    "Error"
                     , Name "GLib"    "HashTable"
                     , Name "GLib"    "List"
                     , Name "GLib"    "SList"
                     , Name "GLib"    "Variant"
                     , Name "GObject" "Value"
                     , Name "GObject" "Closure"
                     ]
          )
        . fst
        )
    $ mapMaybe (traverse dropMovedItems)
            -- Some callback types are defined inside structs
    $ map fixAPIStructs
            -- Try to guess nullability of properties when there is no
            -- nullability info in the GIR.
    $ map guessPropertyNullability
            -- Not every interface providing signals or properties is
            -- correctly annotated as descending from GObject, fix this.
    $ map detectGObject
            -- Some APIs contain duplicated fields by mistake, drop
            -- the duplicates.
    $ map dropDuplicatedFields
            -- Make sure that every argument marked as being a
            -- destructor for a user_data argument has an associated
            -- user_data argument.
    $ map checkClosureDestructors
    $ M.toList
    $ apis

  -- Make sure we generate a "Callbacks" module, since it is imported
  -- by other modules. It is fine if it ends up empty.
  submodule "Callbacks" (return ())

genModule :: M.Map Name API -> CodeGen ()
genModule apis = do
  -- Reexport Data.GI.Base for convenience (so it does not need to be
  -- imported separately).
  -- line "import Data.GI.Base"
  -- exportModule "Data.GI.Base"

  -- Some API symbols are embedded into structures, extract these and
  -- inject them into the set of APIs loaded and being generated.
  let embeddedAPIs =
        (M.fromList . concatMap extractCallbacksInStruct . M.toList) apis
  allAPIs <- getAPIs
  recurseWithAPIs (M.union allAPIs embeddedAPIs)
                  (genModule' (M.union apis embeddedAPIs))
