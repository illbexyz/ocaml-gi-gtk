module Callable
  ( genCCallableWrapper
  , ForeignSymbol(..)
  , skipRetVal
  , arrayLengths
  , arrayLengthsMap
  , fixupCallerAllocates
  , callableHInArgs
  , callableHOutArgs
  , callableOCamlTypes
  , genMlMacro
  )
where

import           Control.Monad                  ( forM
                                                , forM_
                                                , when
                                                , zipWithM
                                                )
import           Data.Bool                      ( bool )
import           Data.List                      ( nub )
import           Data.Maybe                     ( isJust )
import           Data.Monoid                    ( (<>) )
import           Data.Tuple                     ( swap )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

import           API
import           Haddock                        ( writeHaddock
                                                , RelativeDocPosition(..)
                                                , writeArgDocumentation
                                                , writeReturnDocumentation
                                                )
import           Code
import           Conversions
import           SymbolNaming
import           Type
import           Util

import           Text.Show.Pretty               ( ppShow )


-- | Generate a foreign import for the given C symbol. Return the name
-- of the corresponding Haskell identifier.
genOCamlExternal :: Name -> Text -> Callable -> ExcCodeGen ()
genOCamlExternal mn cSymbol callable = do
  line $ "external " <> camelCaseToSnakeCase (lowerName mn) <> " : "
  indent $ do
    argTypes <- callableOCamlTypes callable
    let argTypesStr       = map typeShow argTypes
        (inArgs, outArgs) = splitAt (length argTypesStr - 1) argTypesStr
        outArg            = head outArgs

    -- Input args
    forM_ inArgs (line . (<> " -> "))
    -- Output arg
    line outArg

    let fn       = mlGiPrefix mn cSymbol
    let nativeFn = if length (args callable) > 5 then fn <> "_bc" else ""
    -- TODO: Handle exceptions in some way
    -- when (callableThrows callable) $
    --        line $ padTo 40 "Ptr (Ptr GError) -> " <> "-- error"
    line $ "= " <> addQuote nativeFn <> " " <> addQuote fn
 where
  addQuote "" = ""
  addQuote s  = "\"" <> s <> "\""

foreignArgConverter :: Integer -> Arg -> ExcCodeGen Text
foreignArgConverter i a = do
  -- TODO: probably NULL isn't always the right default value
  --       but it depends on the type of the arg
  conv <- ocamlValueToC $ argType a
  return $ if mayBeNull a then optionVal i conv "NULL" else conv
 where
  optionVal argNum justConv nothingVal =
    let argNumStr = T.pack (show argNum)
    in  let args = T.intercalate ", " [argNumStr, justConv, nothingVal]
        in  "Option_val(arg" <> args <> ") Ignore" -- TODO: Check if this Ignore is working as intended

genMlMacro :: Name -> Text -> Callable -> ExcCodeGen ()
genMlMacro mn cSymbol callable = do
  let nArgs   = length $ args callable
      outArgs = callableHOutArgs callable

  when (any (\a -> direction a == DirectionInout) (args callable))
    $ notImplementedError "genMlMacro: inout parameters are not implemented yet"

  if (not . null) outArgs
    then do
      let inArgs     = callableHInArgs' callable
          numOutArgs = T.pack $ show $ length outArgs
          numInArgs  = T.pack $ show $ length inArgs
          macroName  = "ML_" <> numInArgs <> "in_" <> numOutArgs <> "out ("
      outCTypes    <- mapM (cType . argType) outArgs
      outConvTypes <- mapM
        (\outArg -> cToOCamlValue (mayBeNull outArg) (Just (argType outArg)))
        outArgs
      inArgTypes  <- zipWithM foreignArgConverter [1 ..] inArgs
      retTypeName <- cToOCamlValue (returnMayBeNull callable)
                                   (returnType callable)

      let outArgTypes =
            map (\(x, y) -> x <> ", " <> y) (zip outCTypes outConvTypes)

      cline
        $  macroName
        <> T.toLower (namespace mn)
        <> ", "
        <> cSymbol
        <> ", "
        <> T.intercalate ", " inArgTypes
        <> ", "
        <> T.intercalate ", " outArgTypes
        <> ", "
        <> retTypeName
        <> ")"
    else do
      let macroName = "ML_" <> T.pack (show nArgs) <> " ("
      retTypeName <- cToOCamlValue (returnMayBeNull callable)
                                   (returnType callable)

      argsTypes <- zipWithM foreignArgConverter [1 ..] (args callable)
      let macroArgs = T.intercalate
            ", "
            ([T.toLower $ namespace mn, cSymbol] ++ argsTypes ++ [retTypeName])
      cline $ macroName <> macroArgs <> ")"

  when (length (args callable) > 5) $ do
    let numArgs = T.pack $ show $ length $ args callable
    cline $ "ML_bc" <> numArgs <> " (" <> mlGiPrefix mn cSymbol <> ")"

-- Given a callable, return a list of (array, length) pairs, where in
-- each pair "length" is the argument holding the length of the
-- (non-zero-terminated, non-fixed size) C array.
arrayLengthsMap :: Callable -> [(Arg, Arg)] -- List of (array, length)
arrayLengthsMap callable = go (args callable) []
 where
  go :: [Arg] -> [(Arg, Arg)] -> [(Arg, Arg)]
  go []       acc = acc
  go (a : as) acc = case argType a of
    TCArray False fixedSize length _ -> if fixedSize > -1 || length == -1
      then go as acc
      else go as $ (a, (args callable) !! length) : acc
    _ -> go as acc

-- Return the list of arguments of the callable that contain length
-- arguments, including a possible length for the result of calling
-- the function.
arrayLengths :: Callable -> [Arg]
arrayLengths callable =
  map snd (arrayLengthsMap callable) <>
               -- Often one of the arguments is just the length of
               -- the result.
                                        case returnType callable of
    Just (TCArray False (-1) length _) ->
      if length > -1 then [(args callable) !! length] else []
    _ -> []

-- This goes through a list of [(a,b)], and tags every entry where the
-- "b" field has occurred before with the value of "a" for which it
-- occurred. (The first appearance is not tagged.)
classifyDuplicates :: Ord b => [(a, b)] -> [(a, b, Maybe a)]
classifyDuplicates args = doClassify Map.empty args
 where
  doClassify :: Ord b => Map.Map b a -> [(a, b)] -> [(a, b, Maybe a)]
  doClassify _ [] = []
  doClassify found ((value, key) : args) =
    (value, key, Map.lookup key found)
      : doClassify (Map.insert key value found) args

-- | Whether to skip the return value in the generated bindings. The
-- C convention is that functions throwing an error and returning
-- a gboolean set the boolean to TRUE iff there is no error, so
-- the information is always implicit in whether we emit an
-- exception or not, so the return value can be omitted from the
-- generated bindings without loss of information (and omitting it
-- gives rise to a nicer API). See
-- https://bugzilla.gnome.org/show_bug.cgi?id=649657
skipRetVal :: Callable -> Bool
skipRetVal callable =
  (skipReturn callable)
    || (callableThrows callable && returnType callable == Just
         (TBasicType TBoolean)
       )

-- Find the association between closure arguments and their
-- corresponding callback.
closureToCallbackMap :: Callable -> ExcCodeGen (Map.Map Int Arg)
closureToCallbackMap callable =
    -- The introspection info does not specify the closure for destroy
    -- notify's associated with a callback, since it is implicitly the
    -- same one as the ScopeTypeNotify callback associated with the
    -- DestroyNotify.
                                go
  (filter (not . (`elem` destroyers)) $ args callable)
  Map.empty

 where
  destroyers =
    map (args callable !!) . filter (/= -1) . map argDestroy $ args callable

  go :: [Arg] -> Map.Map Int Arg -> ExcCodeGen (Map.Map Int Arg)
  go []         m = return m
  go (arg : as) m = if argScope arg == ScopeTypeInvalid
    then go as m
    else case argClosure arg of
      (-1) -> go as m
      c    -> case Map.lookup c m of
        Just _ ->
          notImplementedError
            $  "Closure for multiple callbacks unsupported"
            <> T.pack (ppShow arg)
            <> "\n"
            <> T.pack (ppShow callable)
        Nothing -> go as $ Map.insert c arg m

-- user_data style arguments.
prepareClosures :: Callable -> Map.Map Text Text -> ExcCodeGen ()
prepareClosures callable nameMap = do
  m <- closureToCallbackMap callable
  let closures = filter (/= -1) . map argClosure $ args callable
  forM_ closures $ \closure -> case Map.lookup closure m of
    Nothing ->
      badIntroError
        $  "Closure not found! "
        <> T.pack (ppShow callable)
        <> "\n"
        <> T.pack (ppShow m)
        <> "\n"
        <> tshow closure
    Just cb -> do
      let closureName = escapedArgName $ (args callable) !! closure
          n           = escapedArgName cb
      n' <- case Map.lookup n nameMap of
        Just n -> return n
        Nothing ->
          badIntroError
            $  "Cannot find closure name!! "
            <> T.pack (ppShow callable)
            <> "\n"
            <> T.pack (ppShow nameMap)
      case argScope cb of
        ScopeTypeInvalid ->
          badIntroError $ "Invalid scope! " <> T.pack (ppShow callable)
        ScopeTypeNotified -> do
          line $ "let " <> closureName <> " = castFunPtrToPtr " <> n'
          case argDestroy cb of
            (-1) ->
              badIntroError $ "ScopeTypeNotified without destructor! " <> T.pack
                (ppShow callable)
            k ->
              let destroyName = escapedArgName $ (args callable) !! k
              in  line $ "let " <> destroyName <> " = safeFreeFunPtrPtr"
        ScopeTypeAsync -> line $ "let " <> closureName <> " = nullPtr"
        ScopeTypeCall  -> line $ "let " <> closureName <> " = nullPtr"

freeCallCallbacks :: Callable -> Map.Map Text Text -> ExcCodeGen ()
freeCallCallbacks callable nameMap = forM_ (args callable) $ \arg -> do
  let name = escapedArgName arg
  name' <- case Map.lookup name nameMap of
    Just n -> return n
    Nothing ->
      badIntroError
        $  "Could not find "
        <> name
        <> " in "
        <> T.pack (ppShow callable)
        <> "\n"
        <> T.pack (ppShow nameMap)
  when (argScope arg == ScopeTypeCall)
    $  line
    $  "safeFreeFunPtr $ castFunPtrToPtr "
    <> name'

-- | Name for the first argument in dynamic wrappers (the `FunPtr`).
funPtr :: Text
funPtr = "__funPtr"

-- | "In" arguments for the given callable on the Haskell side,
-- together with the omitted arguments.
callableHInArgs :: Callable -> ExposeClosures -> ([Arg], [Arg])
callableHInArgs callable expose =
  let
    inArgs = filter ((/= DirectionOut) . direction) $ args callable
             -- We do not expose user_data arguments,
             -- destroynotify arguments, and C array length
             -- arguments to Haskell code.
    closures =
      map (args callable !!) . filter (/= -1) . map argClosure $ inArgs
    destroyers =
      map (args callable !!) . filter (/= -1) . map argDestroy $ inArgs
    omitted = case expose of
      WithoutClosures -> arrayLengths callable <> closures <> destroyers
      WithClosures    -> arrayLengths callable
  in
    (filter (`notElem` omitted) inArgs, omitted)

callableHInArgs' :: Callable -> [Arg]
callableHInArgs' c = filter (\a -> direction a /= DirectionOut) (args c)

-- | "Out" arguments for the given callable on the Haskell side.
callableHOutArgs :: Callable -> [Arg]
callableHOutArgs callable =
  let outArgs = filter ((/= DirectionIn) . direction) $ args callable
  in  filter (`notElem` (arrayLengths callable)) outArgs

callableOCamlTypes :: Callable -> ExcCodeGen [TypeRep]
callableOCamlTypes c = do
  inArgs  <- mapM argToTyperep $ callableHInArgs' c
  outArgs <- mapM argToTyperep $ callableHOutArgs c
  retType <- case returnType c of
    Nothing -> return $ con0 "unit"
    Just t  -> outParamOcamlType t

  let optionalRetType = if returnMayBeNull c then option retType else retType

  let outArgs' = case outArgs of
        [] -> optionalRetType
        _  -> tuple $ optionalRetType : outArgs

  return $ inArgs ++ [outArgs']

 where
  argToTyperep a = do
    ocamlType <- haskellType $ argType a
    return $ if mayBeNull a then option ocamlType else ocamlType

-- | Invoke the given C function, taking care of errors.
invokeCFunction :: Callable -> ForeignSymbol -> [Text] -> CodeGen ()
invokeCFunction callable symbol argNames = do
  let returnBind = case returnType callable of
        Nothing -> ""
        _       -> if skipRetVal callable then "_ <- " else "result <- "
      maybeCatchGErrors =
        if callableThrows callable then "propagateGError $ " else ""
      call = case symbol of
        KnownForeignSymbol s -> s
        DynamicForeignSymbol w ->
          parenthesize (dynamicWrapper w <> " " <> funPtr)
  line
    $  returnBind
    <> maybeCatchGErrors
    <> call
    <> (T.concat . map (" " <>)) argNames

-- | Return the result of the call, possibly including out arguments.
returnResult :: Callable -> Text -> [Text] -> CodeGen ()
returnResult callable result pps =
  if skipRetVal callable || returnType callable == Nothing
    then case pps of
      []        -> line "return ()"
      (pp : []) -> line $ "return " <> pp
      _         -> line $ "return (" <> T.intercalate ", " pps <> ")"
    else case pps of
      [] -> line $ "return " <> result
      _  -> line $ "return (" <> T.intercalate ", " (result : pps) <> ")"

-- | caller-allocates arguments are arguments that the caller
-- allocates, and the called function modifies. They are marked as
-- 'out' argumens in the introspection data, we sometimes treat them
-- as 'inout' arguments instead. The semantics are somewhat tricky:
-- for memory management purposes they should be treated as "in"
-- arguments, but from the point of view of the exposed API they
-- should be treated as "out" or "inout". Unfortunately we cannot
-- always just assume that they are purely "out", so in many cases the
-- generated API is somewhat suboptimal (since the initial values are
-- not important): for example for g_io_channel_read_chars the size of
-- the buffer to read is determined by the caller-allocates
-- argument. As a compromise, we assume that we can allocate anything
-- that is not a TCArray of length determined by an argument.
fixupCallerAllocates :: Callable -> Callable
fixupCallerAllocates c = c { args = map (fixupLength . fixupDir) (args c) }
 where
  fixupDir :: Arg -> Arg
  fixupDir a = case argType a of
    TCArray _ _ l _ -> if argCallerAllocates a && l > -1
      then a { direction = DirectionInout, transfer = TransferEverything }
      else a
    _ -> a

  lengthsMap :: Map.Map Arg Arg
  lengthsMap = Map.fromList (map swap (arrayLengthsMap c))

  -- Length arguments of caller-allocates arguments should be
  -- treated as "in".
  fixupLength :: Arg -> Arg
  fixupLength a = case Map.lookup a lengthsMap of
    Nothing -> a
    Just array ->
      if argCallerAllocates array then a { direction = DirectionIn } else a

-- | The foreign symbol to wrap. It is either a foreign symbol wrapped
-- in a foreign import, in which case we are given the name of the
-- Haskell wrapper, or alternatively the information about a "dynamic"
-- wrapper in scope.
data ForeignSymbol = KnownForeignSymbol Text -- ^ Haskell symbol in scope.
                   | DynamicForeignSymbol DynamicWrapper
                     -- ^ Info about the dynamic wrapper.

-- | Information about a dynamic wrapper.
data DynamicWrapper = DynamicWrapper {
      dynamicWrapper :: Text    -- ^ Haskell dynamic wrapper
    , dynamicType    :: Text    -- ^ Name of the type synonym for the
                                -- type of the function to be wrapped.
    }

-- | Some debug info for the callable.
_genCallableDebugInfo :: Callable -> CodeGen ()
_genCallableDebugInfo callable = group $ do
  commentShow "Args"       (args callable)
  commentShow "Lengths"    (arrayLengths callable)
  commentShow "returnType" (returnType callable)
  commentLine $ "throws : " <> (tshow $ callableThrows callable)
  commentLine $ "Skip return : " <> (tshow $ skipReturn callable)
  when
      (skipReturn callable && returnType callable /= Just (TBasicType TBoolean))
    $ do
        commentLine "XXX return value ignored, but it is not a boolean."
        commentLine "    This may be a memory leak?"
 where
  commentShow :: Show a => Text -> a -> CodeGen ()
  commentShow prefix s =
    let padding = T.replicate (T.length prefix + 2) " "
        padded  = case T.lines (T.pack $ ppShow s) of
          [] -> []
          (f : rest) ->
            "-- " <> prefix <> ": " <> f : map (("-- " <> padding) <>) rest
    in  mapM_ commentLine padded

-- | Generate a wrapper for a known C symbol.
genCCallableWrapper :: Name -> Text -> Callable -> ExcCodeGen ()
genCCallableWrapper mn cSymbol callable = do

  -- genCallableDebugInfo callable

  let callable' = fixupCallerAllocates callable

  genOCamlExternal mn cSymbol callable'

  blank

  genMlMacro mn cSymbol callable'
  -- deprecatedPragma (lowerName mn) (callableDeprecated callable)
  -- writeDocumentation DocBeforeSymbol (callableDocumentation callable)
  -- void (genHaskellWrapper mn (KnownForeignSymbol hSymbol) callable'
  --        WithoutClosures)

-- | For callbacks we do not need to keep track of which arguments are
-- closures.
forgetClosures :: Callable -> Callable
forgetClosures c = c { args = map forgetClosure (args c) }
 where
  forgetClosure :: Arg -> Arg
  forgetClosure arg = arg { argClosure = -1 }
