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
                                                , Type(..)
                                                )
import           Callable                       ( genCCallableWrapper
                                                , callableOCamlTypes
                                                )
import           Code                           ( CodeGen
                                                , ExcCodeGen
                                                , gline
                                                , getFreshTypeVariable
                                                , currentNS
                                                )
import           GObject                        ( isGObject )
import           Naming
import           TypeRep
import           Util                           ( noLast )


data MethodInArg = BasicIn Text Text
                 | ClassType Bool Text Name
  deriving (Show)

data MethodOutArg = BasicOut Text
                  | Class Bool Name
  deriving (Show)

type MethodArgs = (MethodInArg, [MethodInArg], MethodOutArg)

methodInTypeShow :: Text -> TypeRep -> CodeGen MethodInArg
methodInTypeShow currNS (OptionCon (ObjCon (TypeVarCon tvar (RowCon More (PolyCon (NameCon n@(Name "Gtk" _)))))))
  = return $ ClassType True tvar n
methodInTypeShow currNS (ObjCon (TypeVarCon tvar (RowCon More (PolyCon (NameCon n@(Name "Gtk" _))))))
  = return $ ClassType False tvar n
methodInTypeShow currNS t = do
  tvar <- getFreshTypeVariable
  return $ BasicIn tvar $ methodTypeShow currNS t

methodOutTypeShow :: Text -> TypeRep -> CodeGen MethodOutArg
methodOutTypeShow currNS (OptionCon (ObjCon (TypeVarCon tvar (RowCon Less (PolyCon (NameCon n@(Name "Gtk" _)))))))
  = return $ Class True n
methodOutTypeShow currNS (ObjCon (TypeVarCon tvar (RowCon Less (PolyCon (NameCon n@(Name "Gtk" _))))))
  = return $ Class False n
methodOutTypeShow currNS t = do
  tvar <- getFreshTypeVariable
  return $ BasicOut $ methodTypeShow currNS t

containsClassTypes :: [MethodInArg] -> Bool
containsClassTypes []   = False
containsClassTypes args = True `elem` map inner args
 where
  inner (BasicIn _ _    ) = False
  inner (ClassType _ _ _) = True

returnsClass :: [MethodOutArg] -> Bool
returnsClass []   = False
returnsClass args = isClass (last args)
 where
  isClass (Class _ _) = True
  isClass _           = False

addOption :: Bool -> Text -> Text
addOption False t = t
addOption True  t = t <> " option"

showMethodInArg :: MethodInArg -> CodeGen Text
showMethodInArg (BasicIn _ t              ) = return t
showMethodInArg (ClassType isOption tvar n) = do
  currNS <- currentNS
  return
    $  addOption isOption
    $  "(#"
    <> nsOCamlType currNS n
    <> "_o as '"
    <> tvar
    <> ")"

showMethodOutArg :: MethodOutArg -> CodeGen Text
showMethodOutArg (BasicOut t      ) = return t
showMethodOutArg (Class isOption n) = do
  currNS <- currentNS
  return $ addOption isOption $ nsOCamlClass currNS n

extractClassVars :: [MethodInArg] -> [Text]
extractClassVars = concatMap inner
 where
  inner (ClassType _ var _) = [var]
  inner (BasicIn var _    ) = []

extractVars :: [MethodInArg] -> [Text]
extractVars = concatMap inner
 where
  inner (ClassType _ var _) = [var]
  inner (BasicIn var _    ) = [var]

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

genMethod :: Name -> Method -> ExcCodeGen ()
genMethod cn Method { methodName = mn, methodSymbol = sym, methodCallable = c, methodType = t }
  = when (t /= Constructor) $ do
    currNS <- currentNS
    let mName = escapeOCamlReserved $ name mn
    -- export (NamedSubsection MethodSection $ lowerName mn) (lowerName mn')
    returnsGObject <- maybe (return False) isGObject (returnType c)

    -- commentLine $ "method " <> name' <> "::" <> mName
    -- commentLine $ "method type : " <> tshow t

    let c' = if Constructor == t
          then fixConstructorReturnType returnsGObject cn c
          else c
        c'' = if OrdinaryMethod == t then fixMethodArgs c' else c'

    genCCallableWrapper mn sym c''

    typeReps     <- callableOCamlTypes c''
    mbMethodArgs <- typeRepsToMethodArgs typeReps

    case mbMethodArgs of
      Nothing                       -> return ()
      Just args@(_, inArgs, outArg) -> do
        let tVars     = methodTVars typeReps
            tVarsText = case tVars of
              []   -> ""
              vars -> T.intercalate " " (("'" <>) <$> vars) <> "."

        methodSig  <- methodSignature tVarsText args
        methodBody <- methodBody tVars mName args

        gline $ "  method " <> mName <> methodSig <> " = "
        gline $ "    " <> bodyPrefix args <> methodBody
        gline ""

 where
  methodTVars :: [TypeRep] -> [Text]
  methodTVars []             = []
  methodTVars (_ : typeReps) = concatMap getVars typeReps

  methodSignature :: Text -> MethodArgs -> CodeGen Text
  methodSignature argVars (headArg, inArgs, outArg) = do
    inArgsShow  <- mapM showMethodInArg inArgs
    outShow     <- showMethodOutArg outArg
    headArgShow <- case headArg of
      ClassType _ _ _ -> return ""
      _               -> (<> " -> ") <$> showMethodInArg headArg
    let argsShow = inArgsShow ++ [outShow]
    return $ " : " <> argVars <> headArgShow <> T.intercalate " -> " argsShow

  bodyPrefix :: MethodArgs -> Text
  bodyPrefix (_, inArgs, _) = do
    let vars = extractVars inArgs
    case vars of
      [] -> ""
      _  -> "fun " <> T.intercalate " " vars <> " -> "

  methodBody :: [Text] -> Text -> MethodArgs -> CodeGen Text
  methodBody _ mName mArgs@(_, _, Class False n) = do
    currNS <- currentNS
    return
      $  "new "
      <> nsOCamlClass currNS n
      <> " ("
      <> boundMethod mName mArgs
      <> ")"
  methodBody _ mName mArgs@(_, _, Class True n) = do
    currNS <- currentNS
    return
      $  "Option.map (new "
      <> nsOCamlClass currNS n
      <> ") ("
      <> boundMethod mName mArgs
      <> ")"
  methodBody _ mName mArgs = return $ boundMethod mName mArgs

  boundMethod :: Text -> MethodArgs -> Text
  boundMethod mName (headArg, inArgs, outArg) = do
    let obj = case headArg of
          ClassType _ _ _ -> " obj "
          _               -> " "
    name cn <> "." <> mName <> obj <> T.intercalate
      " "
      (mapInArg outArg <$> inArgs)
   where
    mapInArg _ (ClassType False tvar n) = tvar <> "#as_" <> ocamlIdentifier n
    mapInArg _ (ClassType True tvar n) =
      "(Option.map (fun z -> z#as_" <> ocamlIdentifier n <> ") " <> tvar <> ")"
    mapInArg _ (BasicIn tvar _) = tvar

  typeRepsToMethodArgs :: [TypeRep] -> CodeGen (Maybe MethodArgs)
  typeRepsToMethodArgs []                    = return Nothing
  typeRepsToMethodArgs xs@(h : typeRepsTail) = do
    currNS      <- currentNS
    headArg     <- methodInTypeShow currNS h
    inTypeTexts <- mapM (methodInTypeShow currNS) (noLast typeRepsTail)
    let retTypeRep = last xs
    retTypeText <- methodOutTypeShow currNS retTypeRep
    return $ Just (headArg, inTypeTexts, retTypeText)
