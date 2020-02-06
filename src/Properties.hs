module Properties
  ( genInterfaceProperties
  , genObjectProperties
  )
where

import           Control.Applicative            ( (<$>) )
import           Control.Monad                  ( forM_
                                                , when
                                                , unless
                                                , forM
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           API
import           Haddock                        ( addSectionDocumentation )
import           Conversions
import           Code
import           GObject
import           Inheritance                    ( fullObjectPropertyList
                                                , fullInterfacePropertyList
                                                , instanceTree
                                                )
import           Naming
import           QualifiedNaming                ( qualifiedSymbol )
import           TypeRep
import           Util

isPropertyInParents :: Name -> Text -> CodeGen Bool
isPropertyInParents cn pName = do
  parents        <- instanceTree cn
  parentsHasProp <- forM parents $ \parentName -> do
    api <- findAPIByName parentName
    return $ case api of
      APIObject o -> do
        let parentPropNames   = (propName <$> objProperties o)
            parentMethodNames = (name . methodName <$> objMethods o)
            parentNames       = parentPropNames ++ parentMethodNames
        pName `elem` parentNames
      _ -> False
  return $ True `elem` parentsHasProp

genPropertySetter :: Text -> Name -> Property -> CodeGen ()
genPropertySetter setter classe prop = do
  alreadyDefProp <- isPropertyInParents classe ("set_" <> propName prop)
  let setterDecl = if alreadyDefProp
        then setter <> "_" <> ocamlIdentifier classe
        else setter
  gline
    $  "  method set_"
    <> setterDecl
    <> " = Gobject.set "
    <> name classe
    <> ".P."
    <> setter
    <> " obj"

genPropertyGetter :: Text -> Name -> Property -> CodeGen ()
genPropertyGetter getter classe prop = do
  alreadyDefProp <- isPropertyInParents classe ("get_" <> propName prop)
  let getterDecl = if alreadyDefProp
        then getter <> "_" <> ocamlIdentifier classe
        else getter
  gline
    $  "  method get_"
    <> getterDecl
    <> " = Gobject.get "
    <> name classe
    <> ".P."
    <> getter
    <> " obj"

genPropertyOCaml :: Name -> Property -> ExcCodeGen ()
genPropertyOCaml classe prop = do
  currNS <- currentNS
  let pName       = propName prop
      uScoresName = escapeOCamlReserved $ hyphensToUnderscores pName
      classType   = typeShow currNS $ RowCon More $ PolyCon $ NameCon classe
      isNullable  = fromMaybe False $ propReadNullable prop
  -- TODO: uncomment next line
  -- writeHaddock DocBeforeSymbol (getterDoc n prop) 
  ocamlConverter <- ocamlDataConv isNullable (propType prop)
  line $ "let " <> uScoresName <> " : " <> "(" <> classType <> ",_) property ="
  indent
    $  line
    $  "{"
    <> "name=\""
    <> pName
    <> "\"; "
    <> "conv="
    <> ocamlConverter
    <> "}"

-- | The property name as a lexically valid Haskell identifier. Note
-- that this is not escaped, since it is assumed that it will be used
-- with a prefix, so if a property is named "class", for example, this
-- will return "class".
-- TODO: this is useless, remove it or convert it for OCaml
hPropName :: Property -> Text
hPropName = lcFirst . hyphensToCamelCase . propName

genObjectProperties :: Name -> Object -> CodeGen ()
genObjectProperties n o = do
  isGO <- apiIsGObject n (APIObject o)
  -- We do not generate bindings for objects not descending from GObject.
  when isGO $ do
    allProps <- fullObjectPropertyList n o >>= mapM
      (\(owner, prop) -> do
        pi <- infoType owner prop
        return $ "'(\"" <> hPropName prop <> "\", " <> pi <> ")"
      )
    genProperties n (objProperties o) allProps

genInterfaceProperties :: Name -> Interface -> CodeGen ()
genInterfaceProperties n iface = do
  allProps <- fullInterfacePropertyList n iface >>= mapM
    (\(owner, prop) -> do
      pi <- infoType owner prop
      return $ "'(\"" <> hPropName prop <> "\", " <> pi <> ")"
    )
  genProperties n (ifProperties iface) allProps

-- If the given accesor is available (indicated by available == True),
-- generate a fully qualified accesor name, otherwise just return
-- "undefined". accessor is "get", "set" or "construct"
accessorOrUndefined :: Bool -> Name -> Text -> CodeGen Text
accessorOrUndefined available owner cName = if not available
  then return "undefined"
  else escapeOCamlReserved <$> qualifiedSymbol cName owner

-- | The name of the type encoding the information for the property of
-- the object.
infoType :: Name -> Property -> CodeGen Text
infoType owner prop =
  let infoType =
          upperName owner
            <> (hyphensToCamelCase . propName) prop
            <> "PropertyInfo"
  in  qualifiedSymbol infoType owner

genOneProperty :: Name -> Property -> ExcCodeGen ()
genOneProperty owner prop = do
  let name       = upperName owner
      cName      = (hyphensToCamelCase . propName) prop
      docSection = NamedSubsection PropertySection (lcFirst cName)
      pName      = name <> cName
      flags      = propFlags prop
      writable =
        PropertyWritable `elem` flags && (PropertyConstructOnly `notElem` flags)
      readable      = PropertyReadable `elem` flags
      constructOnly = PropertyConstructOnly `elem` flags

  addSectionDocumentation docSection (propDoc prop)

  -- For properties the meaning of having transfer /= TransferNothing
  -- is not clear (what are the right semantics for GValue setters?),
  -- and the other possibilities are very uncommon, so let us just
  -- assume that TransferNothing is always the case.
  when (propTransfer prop /= TransferNothing)
    $  notImplementedError
    $  "Property "
    <> pName
    <> " has unsupported transfer type "
    <> tshow (propTransfer prop)

  -- isNullable <- typeIsNullable (propType prop)

  unless (readable || writable || constructOnly)
    $  notImplementedError
    $  "Property is not readable, writable, or constructible: "
    <> tshow pName

  -- TODO: uncomment next line
  -- group $ do
  --   commentLine $ "VVV Prop \"" <> propName prop <> "\""
  --   commentLine $ "   Type: " <> tshow (propType prop)
  --   commentLine $ "   Flags: " <> tshow (propFlags prop)
  --   commentLine $ "   Nullable: " <> tshow (propReadNullable prop,
  --                                       propWriteNullable prop)

  getter <- accessorOrUndefined readable owner cName
  setter <- accessorOrUndefined writable owner cName

  -- constructor <- accessorOrUndefined (writable || constructOnly)
  --                owner cName
  -- clear <- accessorOrUndefined (isNullable && writable &&
  --                               propWriteNullable prop /= Just False)
  --          owner cName

  genPropertyOCaml owner prop
  when (getter /= "undefined") $ genPropertyGetter getter owner prop
  when (setter /= "undefined") $ genPropertySetter setter owner prop
  -- when (constructor /= "undefined") $
  --      genPropertyConstructor constructor owner docSection prop
  -- when (clear /= "undefined") $ genPropertyClear clear owner docSection prop

  -- outType <- if not readable
  --            then return "()"
  --            else do
  --              sOutType <- if isNullable && propReadNullable prop /= Just False
  --                          then typeShow . maybeT <$> isoHaskellType (propType prop)
  --                          else typeShow <$> isoHaskellType (propType prop)
  --              return $ if T.any (== ' ') sOutType
  --                       then parenthesize sOutType
  --                       else sOutType

genMakeParams :: Name -> [Property] -> CodeGen ()
genMakeParams className props = do
  let constructors = filter isConstructor props
  if not $ null constructors
    then do
      let constructorNames = map (escapeOCamlReserved . propName) constructors
          underlinedConstrNames = map hyphensToUnderscores constructorNames
          optionalArgs = "?" <> T.intercalate " ?" underlinedConstrNames
      blank
      line $ "let make_params ~cont pl " <> optionalArgs <> " ="
      indent $ do
        let numConstructors = length underlinedConstrNames
            firstConstrs    = take (numConstructors - 1) underlinedConstrNames
            lastConstr      = last underlinedConstrNames
        line "let pl = ("
        indent $ do
          let mayCons constrName =
                "may_cons P." <> constrName <> " " <> constrName
          forM_ firstConstrs $ \cName -> line $ mayCons cName <> " ("
          line
            $  mayCons lastConstr
            <> " pl"
            <> T.pack (replicate numConstructors ')')
            <> " in"
        line "cont pl"
    else do
      parents <- instanceTree className
      unless (null parents) $ do
        let parent = head parents
        line $ case parent of
          Name "GObject" "Object" -> emptyMake
          Name "Gtk"     "Widget" -> emptyMake
          -- Name "Gtk"     "Container" -> emptyMake
          Name "Gtk"     _        -> inheritedMake parent
          Name _         _        -> emptyMake
 where
  emptyMake = "let make_params ~cont pl () = cont pl"
  inheritedMake parent = "let make_params = " <> name parent <> ".make_params"
  isConstructor prop =
    PropertyConstructOnly
      `elem` propFlags prop
      ||     PropertyConstruct
      `elem` propFlags prop

genProperties :: Name -> [Property] -> [Text] -> CodeGen ()
genProperties n ownedProps _allProps = do
  line "let may_cons = Property.may_cons"
  line "let may_cons_opt = Property.may_cons_opt"
  blank
  gline "  (* Properties *)"
  line "module P = struct"
  indent $ do
    let name = upperName n

    forM_ ownedProps $ \prop -> handleCGExc
      (\err ->
        commentLine
          $  "XXX Generation of property \""
          <> propName prop
          <> "\" of object \""
          <> name
          <> "\" failed: "
          <> describeCGError err
      )
      (genOneProperty n prop)

  line "end"
  genMakeParams n ownedProps
