module Method
    ( genMethod
    )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad                  ( when
                                                , forM
                                                )

import           API                            ( Arg(..)
                                                , Callable(..)
                                                , Direction(..)
                                                , Name(..)
                                                , Method(..)
                                                , MethodType(..)
                                                )
import           Callable                       ( genCCallableWrapper
                                                , callableOCamlTypes
                                                )
import           Code                           ( ExcCodeGen
                                                , gline
                                                , getFreshTypeVariable
                                                )
import           GObject                        ( isGObject )
import           SymbolNaming                   ( camelCaseToSnakeCase )
import           Type

-- | When parsing the GIR file we add the implicit object argument to
-- methods of an object.  Since we are prepending an argument we need
-- to adjust the offset of the length arguments of CArrays, and
-- closure and destroyer offsets.
fixMethodArgs :: Callable -> Callable
fixMethodArgs c = c { args = args'', returnType = returnType' }
  where
    returnType' = fmap fixCArrayLength (returnType c)
    args'       = map (fixDestroyers . fixClosures . fixLengthArg) (args c)
    args''      = fixInstance (head args') : tail args'

    fixLengthArg :: Arg -> Arg
    fixLengthArg arg = arg { argType = fixCArrayLength (argType arg) }

    fixCArrayLength :: Type -> Type
    fixCArrayLength (TCArray zt fixed length t) = if length > -1
        then TCArray zt fixed (length + 1) t
        else TCArray zt fixed length t

    fixCArrayLength t = t

    fixDestroyers :: Arg -> Arg
    fixDestroyers arg =
        let destroy = argDestroy arg
        in  if destroy > -1 then arg { argDestroy = destroy + 1 } else arg

    fixClosures :: Arg -> Arg
    fixClosures arg =
        let closure = argClosure arg
        in  if closure > -1 then arg { argClosure = closure + 1 } else arg

    -- We always treat the instance argument of a method as non-null
    -- and "in", even if sometimes the introspection data may say
    -- otherwise.
    fixInstance :: Arg -> Arg
    fixInstance arg = arg { mayBeNull = False, direction = DirectionIn }

-- For constructors we want to return the actual type of the object,
-- rather than a generic superclass (so Gtk.labelNew returns a
-- Gtk.Label, rather than a Gtk.Widget)
fixConstructorReturnType :: Bool -> Name -> Callable -> Callable
fixConstructorReturnType returnsGObject cn c = c { returnType = returnType' }
  where
    returnType' = if returnsGObject then Just (TInterface cn) else returnType c

handleOptional
    :: TypeRep
    -> Text       -- ^ The function that maps the optional value
    -> Text       -- ^ The value
    -> Text       -- ^ The default value when t is not an optional
    -> Text
handleOptional t fn val _ | isOptional t =
    "(Option.map (" <> fn <> ") (" <> val <> "))"
handleOptional _ _ _ default_ = default_

genMethod :: Name -> Method -> ExcCodeGen ()
genMethod cn Method { methodName = mn, methodSymbol = sym, methodCallable = c, methodType = t }
    = when (t /= Constructor) $ do
        -- export (NamedSubsection MethodSection $ lowerName mn) (lowerName mn')
        returnsGObject <- maybe (return False) isGObject (returnType c)

        -- commentLine $ "method " <> name' <> "::" <> name mn
        -- commentLine $ "method type : " <> tshow t

        let c' = if Constructor == t
                then fixConstructorReturnType returnsGObject cn c
                else c
            c'' = if OrdinaryMethod == t then fixMethodArgs c' else c'

        genCCallableWrapper mn sym c''

        typeReps <- callableOCamlTypes c''

        case typeReps of
            -- The method has no arguments
            [] -> gline $ "  method " <> name mn <> " = " <> boundMethod
            (_ : typeReps') -> do
                let retType    = last typeReps
                    inReps     = take (length typeReps' - 1) typeReps'
                    inReps'    = map (mapPolyToClass . showTypeVar) inReps
                    outRep     = showTypeVar retType
                    typeReps'' = inReps' ++ [outRep]
                    typesStr   = map typeShowWithClass typeReps''
                    typeVars   = map (\t -> "'" <> t <> ".")
                        $ concatMap varsInTypeRep typeReps''
                if isHighLevelObj retType
                    && isInThisNamespace (getTextualCons retType)  -- TODO: isInThisNamespace rejects classes from other libraries
                then
                    gline
                    $  "  method "
                    <> name mn
                    <> " = "
                    <> handleRetVal retType
                else
                    if (length typeReps'' - 1)
                           >  0
                           && any containsClass typeReps''
                        then do
                            funVars <- forM [1 .. (length typeReps'' - 1)]
                                $ const getFreshTypeVariable
                            let inArgs = zipWith mapClasses funVars inReps'
                            gline
                                $  "  method "
                                <> name mn
                                <> " : "
                                <> T.concat typeVars
                                <> T.intercalate " -> " typesStr
                                <> " = fun "
                                <> T.intercalate " " funVars
                                <> " -> "
                                <> boundMethod
                                <> " "
                                <> T.intercalate " " inArgs
                        else
                            gline
                            $  "  method "
                            <> name mn
                            <> " : "
                            <> T.concat typeVars
                            <> T.intercalate " -> " typesStr
                            <> " = "
                            <> boundMethod
  where
    mapClasses typeVar typeRep = if containsClass typeRep
        then handleOptional typeRep
                            ("fun z -> z" <> asClass typeRep)
                            typeVar
                            (typeVar <> asClass typeRep)
        else typeVar

    asClass t = "#as_" <> extractClass t

    handleRetVal t = handleOptional
        t
        ("new " <> classConstructor t)
        boundMethod
        ("new " <> classConstructor t <> " (" <> boundMethod <> ")")

    boundMethod = name cn <> "." <> name mn <> " obj"

    classConstructor t = case className t of
        "Widget"  -> "GObj.widget"
        "Window"  -> "GWindow.window"
        className -> className <> "G." <> camelCaseToSnakeCase className

    isInThisNamespace t = length (T.splitOn "." t) <= 2

    className t = case T.splitOn "." (getTextualCons t) of
        [_nspace, name, _] -> name
        [name, _]          -> name
        _                  -> name cn
