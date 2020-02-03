module CodeGen
  ( genConstant
  , genFunction
  , genModule
  )
where

import           Control.Monad                  ( forM
                                                , forM_
                                                , when
                                                , unless
                                                , filterM
                                                )
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , mapMaybe
                                                , isNothing
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

import           API
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
import           GObject
import           Method                         ( genMethod )
import           Haddock                        ( deprecatedPragma
                                                , addSectionDocumentation
                                                , writeHaddock
                                                , RelativeDocPosition
                                                  ( DocBeforeSymbol
                                                  )
                                                )
import           Object                         ( genObject
                                                , genGObjectCasts
                                                , genInterface
                                                )
import           Struct                         ( extractCallbacksInStruct
                                                , fixAPIStructs
                                                , ignoreStruct
                                                )
import           Code
import           Callable                       ( genCCallableWrapper
                                                , callableOCamlTypes
                                                )
import           Inheritance                    ( instanceTree )
import           Signal                         ( genSignal )
import           Naming
import           QualifiedNaming                   ( submoduleLocation
                                                )
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

-- | Generate wrapper for structures.
genStruct :: Name -> Struct -> CodeGen ()
genStruct n s = unless (ignoreStruct n s) $ do
  let name' = upperName n
  -- writeHaddock DocBeforeSymbol "Memory-managed wrapper type."

  -- addSectionDocumentation ToplevelSection (structDocumentation s)
  addType n Nothing

  hline
    $  "#define "
    <> (namespace n <> name n)
    <> "_val(val) (("
    <> (namespace n <> name n)
    <> "*) MLPointer_val(val))"

  -- if structIsBoxed s
  --   then traceShowM $ "Struct " <> show n <> " is boxed"
  --   else traceShowM $ "Struct " <> show n <> " not boxed"

  -- if structIsBoxed s
  --   then genBoxedObject n (fromJust $ structTypeInit s)
  --   else genWrappedPtr n (structAllocationInfo s) (structSize s)

  -- exportDecl (name' <> "(..)")

  -- -- Generate a builder for a structure filled with zeroes.
  -- genZeroStruct n s

  -- noName name'

  -- -- Generate code for fields.
  -- genStructOrUnionFields n (structFields s)

  -- -- Methods
  -- _methods <- forM (structMethods s) $ \f -> do
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

  return ()

-- | Generated wrapper for unions.
genUnion :: Name -> Union -> CodeGen ()
genUnion n u = do
  let name' = upperName n

  writeHaddock DocBeforeSymbol "Memory-managed wrapper type."

  addSectionDocumentation ToplevelSection (unionDocumentation u)

  -- if unionIsBoxed u
  --   then genBoxedObject n (fromJust $ unionTypeInit u)
  --   else genWrappedPtr n (unionAllocationInfo u) (unionSize u)

  -- Generate code for fields.
  -- genStructOrUnionFields n (unionFields u)

  -- Methods
  _methods <- forM (unionMethods u) $ \f -> do
    let mn = methodName f
    isFunction <- symbolFromFunction (methodSymbol f)
    if not isFunction
      then handleCGExc
        (\e ->
          line
              (  "(* Could not generate method "
              <> name'
              <> "::"
              <> name mn
              <> " *)\n"
              <> "(* Error was : "
              <> describeCGError e
              <> " *)"
              )
            >> return Nothing
        )
        (genMethod n f >> return (Just (n, f)))
      else return Nothing

  return ()

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
genAPI _n (APIUnion     _u) = return ()
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
