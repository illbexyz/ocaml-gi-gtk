module Signal
  ( genSignal
  , genGSignal
  , signalHaskellName
  )
where

import           Control.Monad                  ( forM
                                                , forM_
                                                , when
                                                , unless
                                                )

import           Data.Maybe                     ( catMaybes )
import           Data.Monoid                    ( (<>) )
import           Data.Bool                      ( bool )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

import           Text.Show.Pretty               ( ppShow )

import           API
import           Haddock                        ( deprecatedPragma
                                                , RelativeDocPosition(..)
                                                , writeHaddock
                                                )
import           Data.GI.GIR.Documentation      ( Documentation )

import           Callable                       ( fixupCallerAllocates
                                                , callableHInArgs
                                                , callableHOutArgs
                                                )
import           Code
import           Conversions
import           SymbolNaming
import           Type
import           Util                           ( parenthesize
                                                , withComment
                                                , tshow
                                                , terror
                                                , ucFirst
                                                , prime
                                                )

argsTypeRep :: [Arg] -> ExcCodeGen [Text]
argsTypeRep = mapM (\arg -> ocamlDataConv (mayBeNull arg) (argType arg))

ocamlMarshaller :: [Arg] -> Text -> Text -> ExcCodeGen Text
ocamlMarshaller args sigName onName = case args of
  []    -> return "marshal_unit"
  args' -> do
    let sigName' = "\"" <> ucFirst onName <> "::" <> sigName <> "\""
        len      = length args'
        marsh    = "fun f -> marshal" <> T.pack (show len)
    argTypes <- argsTypeRep args'
    return $ T.intercalate " " (marsh : argTypes ++ [sigName', "f"])

-- | The prototype of the callback on the OCaml side (what users of
-- the binding will see)
genOCamlCallbackPrototype
  :: Text
  -> Callable
  -> Text
  -> Text
  -> ExposeClosures
  -> Documentation
  -> ExcCodeGen ()
genOCamlCallbackPrototype subsec cb _htype classe expose _doc = do
    -- let name' = case expose of
    --               WithClosures -> callbackHTypeWithClosures htype
    --               WithoutClosures -> htype
  let (hInArgs, _) = callableHInArgs cb expose
      -- inArgsWithArrows = zip ("" : repeat "-> ") hInArgs
      -- hOutArgs = callableHOutArgs cb

  -- export (NamedSubsection SignalSection subsec) name'
  -- writeDocumentation DocBeforeSymbol doc

  -- ret <- hOutType cb hOutArgs
  let ocamlClassName = camelCaseToSnakeCase classe
      classType      = typeShow $ poly $ con0 ("`" <> ocamlClassName)

  marshaller <- ocamlMarshaller hInArgs subsec classe

  line
    $  "let "
    <> subsec
    <> " = {"
    <> "name=\""
    <> subsec
    <> "\"; "
    <> "classe="
    <> classType
    <> "; "
    <> "marshaller="
    <> marshaller
    <> "}"

-- Wrap a conversion of a nullable object into "Maybe" object, by
-- checking whether the pointer is NULL.
convertNullable :: Text -> BaseCodeGen e Text -> BaseCodeGen e Text
convertNullable aname c = do
  line $ "maybe" <> ucFirst aname <> " <-"
  indent $ do
    line $ "if " <> aname <> " == nullPtr"
    line "then return Nothing"
    line "else do"
    indent $ do
      unpacked <- c
      line $ "return $ Just " <> unpacked
    return $ "maybe" <> ucFirst aname

-- | Write some simple debug message when signal generation fails, and
-- generate a placeholder SignalInfo instance.
processSignalError :: Signal -> Name -> CGError -> CodeGen ()
processSignalError signal owner err = do
  let qualifiedSignalName = upperName owner <> "::" <> sigName signal
      -- sn = (ucFirst . signalHaskellName . sigName) signal
  line $ T.concat
    [ "(* Could not generate signal "
    , qualifiedSignalName
    , " *)\n"
    , "(* Error was : "
    , describeCGError err
    , " *)"
    ]

-- | Generate a wrapper for a signal.
genSignal :: Signal -> Name -> CodeGen ()
genSignal s@Signal { sigName = sn, sigCallable = cb } on =
  handleCGExc (processSignalError s on) $ do
    let classe              = lowerName on
        sn'                 = signalOCamlName sn
        signalConnectorName = classe <> ucFirst sn'
        cbType              = signalConnectorName <> "Callback"
        -- docSection = NamedSubsection SignalSection $ lcFirst sn'

    -- deprecatedPragma cbType (callableDeprecated cb)

    genOCamlCallbackPrototype sn' cb cbType classe WithoutClosures (sigDoc s)

  -- genCallbackWrapperFactory (lcFirst sn') cbType

  -- if callableThrows cb
  --   then do
  --     line $ "-- No Haskell->C wrapper generated since the function throws."
  --     blank
  --   else do
  --     genClosure (lcFirst sn') cb cbType signalConnectorName True
  --     genCallbackWrapper (lcFirst sn') cb cbType True

genGSignal :: Signal -> Name -> CodeGen ()
genGSignal Signal { sigName = sn, sigCallable = _ } on = do
  let sn' = signalOCamlName sn
      on' = ucFirst $ lowerName on
  gline $ "  method " <> sn' <> " = self#connect " <> on' <> ".S." <> sn'
