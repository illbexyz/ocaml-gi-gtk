module Object
  ( genObject
  , genGObjectCasts
  , genInterface
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad                  ( forM_
                                                , when
                                                , unless
                                                )

import           Data.GI.GIR.BasicTypes         ( Type(TInterface) )

import           API
import           Code
import           GObject
import           Inheritance                    ( instanceTree )
import           Method                         ( genMethod )
import           Naming
import           QualifiedNaming                ( nsOCamlClass )
import           Properties                     ( genObjectProperties
                                                , genInterfaceProperties
                                                )
import           Signal                         ( genSignal
                                                , genGSignal
                                                )
import           Files                          ( excludeFiles )

isSetterOrGetter :: Object -> Method -> Bool
isSetterOrGetter o m =
  let props = objProperties o
  in  let propNames = map (hyphensToUnderscores . propName) props
      in  let mName = name $ methodName m
          in  ("get" `T.isPrefixOf` mName || "set" `T.isPrefixOf` mName)
                && any (`T.isSuffixOf` mName) propNames

genGObjectCasts :: Name -> Text -> Text -> CodeGen ()
genGObjectCasts n ctype checkMacro = unless (n `elem` excludeFiles) $ do
  hline
    ("#define " <> objectVal n <> "(val) check_cast(" <> checkMacro <> ", val)")
  -- hline $ "#define " <> nspace <> n <> "_val(" <> "val) ((" <> x <> "*) val)"
  hline $ "#define " <> valObject n <> " Val_GAnyObject"
  cline ("Make_Val_option(" <> ctype <> "," <> valObject n <> ")")
  hline ("value " <> valOptObject n <> " (" <> ctype <> "*);")
  addCDep (namespace n <> name n)

genSignalClass :: Name -> Object -> CodeGen ()
genSignalClass n o = do
  parents <- instanceTree n

  let parent    = head parents
      ocamlName = ocamlIdentifier n

  gline $ "and " <> ocamlName <> "_signals obj = object (self)"

  -- TODO: The default case should be the commented one but it makes sense 
  --       only when every lib can be generated, otherwise the parent class
  --       is not available.
  --       Then the (Name "Gtk" _) case can be removed 
  parentClass <- nsOCamlClass parent
  let parentSignal = case head parents of
        Name "Gtk"       "Widget" -> "GObj.widget_signals_impl"
        Name "GObject"   "Object" -> "[_] GObj.gobject_signals"
        Name "Gtk"       _        -> parentClass <> "_signals"
        Name "GtkSource" _        -> parentClass <> "_signals"
        _                         -> "[_] GObj.gobject_signals"
        -- _                       -> parentClass <> "_signals" 

  gline $ "  inherit " <> parentSignal <> " obj"

  forM_ (objSignals o) $ \s -> genGSignal s n
  gline "end"
  gblank

cTypeInit :: Text -> Text -> Text
cTypeInit cTypeName typeInit = T.unlines
  [ "CAMLprim value ml_gi" <> cTypeName <> "_init(value unit) {"
  , "    GType t = " <> typeInit <> "();"
  , "    return Val_GType(t);"
  , "}"
  ]

genCObjectTypeInit :: Object -> Name -> CodeGen ()
genCObjectTypeInit Object { objTypeInit = typeInit } (Name ns nm)
  | typeInit /= "" = cline
  $ cTypeInit (camelCaseToSnakeCase (ns <> nm)) typeInit
genCObjectTypeInit _ _ = return ()

genCInterfaceTypeInit :: Interface -> Name -> CodeGen ()
genCInterfaceTypeInit Interface { ifTypeInit = Just typeInit } (Name ns nm) =
  cline $ cTypeInit (camelCaseToSnakeCase (ns <> nm)) typeInit
genCInterfaceTypeInit _ _ = return ()

genMlTypeInit :: Name -> CodeGen ()
genMlTypeInit (Name ns nm) = group $ do
  let namespacedOcamlName = camelCaseToSnakeCase (ns <> nm)
  line
    $  "external ml_gi"
    <> namespacedOcamlName
    <> "_init : unit -> unit = \"ml_gi"
    <> namespacedOcamlName
    <> "_init\""
  line $ "let () = ml_gi" <> namespacedOcamlName <> "_init ()"

-- Every GObject has a macro used to cast a pointer to the type of the GObject
-- e.g.: GtkButton has the GTK_BUTTON macro
-- This macro however can't be retrieved by the GIR.
-- To make this problem even harder, every information we know about the name
-- of the object is in TitleCase, and we cannot obtain the macro name from
-- the Name or the CType, because for some objects we can't find the position
-- of the underlines. (e.g.: UIManager would be converted to U_I_MANAGER instead
-- of UI_MANAGER).
-- 
-- The only attribute we can use is the typeInit. It contains the object name
-- in snake case followed by a "_get_type" suffix.
-- So we remove the suffix and turn to upper case.
getObjCheckMacro :: Object -> Text
getObjCheckMacro o = T.toUpper $ fst $ T.breakOn "_get_type" (objTypeInit o)

-- See getObjCheckMacro. For the interfaces the getTypeInit is optional,
-- so some interfaces might not be properly initialized.
-- #bug?
getIfCheckMacro :: Interface -> Maybe Text
getIfCheckMacro i = do
  typeInit <- ifTypeInit i
  return $ T.toUpper $ fst $ T.breakOn "_get_type" typeInit

genObject :: Name -> Object -> CodeGen ()
genObject n o = do
  isGO <- isGObject (TInterface n)
  if not isGO
    then return ()
    else do
      let objectName = name n
          ocamlName  = escapeOCamlReserved $ camelCaseToSnakeCase objectName

      parents <- instanceTree n

      case parents of
        [] -> addType n Nothing
        _  -> addType n (Just $ head parents)

      forM_ (objCType o)
        $ \ctype -> genGObjectCasts n ctype (getObjCheckMacro o)

      if namespace n /= "Gtk" || n `elem` excludeFiles
        then return ()
        else genObject' n o ocamlName

genObject' :: Name -> Object -> Text -> CodeGen ()
genObject' n o ocamlName = do
  parents <- instanceTree n
  let name'      = upperName n
      nspace     = namespace n
      objectName = name n
      parent     = head parents

  genCObjectTypeInit o n

  gline "open Gobject"
  gblank

  -- TODO: The default case should be the commented one but it makes sense 
  --       only when every lib can be generated, otherwise the parent class
  --       is not available.
  --       Then the (Name "Gtk" _) case can be removed 
  parentClass <- nsOCamlClass parent
  let parentSkelClass = case parent of
        Name "Gtk"       "Widget" -> "['a] GObj.widget_impl"
        Name "GObject"   "Object" -> "GObj.gtkobj"
        Name "Gtk"       _        -> parentClass <> "_skel"
        Name "GtkSource" _        -> parentClass <> "_skel"
        _                         -> "GObj.gtkobj"
        -- _                       -> parentClass <> "_skel"

  gline $ "class " <> ocamlName <> "_skel obj = object (self)"
  gline $ "  inherit " <> parentSkelClass <> " obj"
  gline
    $  "  method as_"
    <> ocamlName
    <> " = (obj :> "
    <> nsOCamlType (namespace n) n
    <> " obj)"

  line "open Gobject"
  line "open Data"
  blank
  blank

  genMlTypeInit n

  group $ do
    genObjectProperties n o
    gblank

  unless (null $ objSignals o) $ group $ do
    line "module S = struct"
    indent $ do
      line "open GtkSignal"
      forM_ (objSignals o) $ \s -> genSignal s n

    line "end"

  group
    $  line
    $  "let cast w : "
    <> nsOCamlType (namespace n) n
    <> " obj = try_cast w \""
    <> nspace
    <> objectName
    <> "\""

  group
    $  line
    $  "let create pl : "
    <> nsOCamlType (namespace n) n
    <> " obj = GtkObject.make \""
    <> nspace
    <> objectName
    <> "\" pl"

  -- Methods
  gline "  (* Methods *)"
  let methods  = objMethods o
  let methods' = filter (not . isSetterOrGetter o) methods

  forM_ methods' $ \f -> do
    let mn = methodName f
    handleCGExc
      (\e -> line
        (  "(* Could not generate method "
        <> name'
        <> "::"
        <> name mn
        <> " *)\n"
        <> "(* Error was : "
        <> describeCGError e
        <> " *)"
        )
      )
      (genMethod n f)
  gline "end"
  gblank

  genSignalClass n o

  gline $ "and " <> ocamlName <> " obj = object (self)"
  gline $ "  inherit " <> ocamlName <> "_skel obj"
  gline $ "  method connect = new " <> ocamlName <> "_signals obj"
  gline "end"
  gblank

  genObjectConstructor n ocamlName

genObjectConstructor :: Name -> Text -> CodeGen ()
genObjectConstructor n ocamlName = do
  currNS  <- currentNS
  parents <- instanceTree n

  let makeParamsParents = filter (isMakeParamsParent currNS) $ reverse parents
      mkParentsNum      = length makeParamsParents

  gline $ "let " <> ocamlName <> " = "

  forM_ (zip makeParamsParents [0 ..])
    $ \(p, idx) -> gline $ makeParamsCont p idx
  gline $ makeParamsCont n mkParentsNum

  -- If Widget is a parent then we need to add the ~packing and ~show labels
  gline $ indentBy (mkParentsNum + 2) <> if Name "Gtk" "Widget" `elem` parents
    then "fun pl ?packing ?show () -> GObj.pack_return ("
    else "fun pl () -> ("

  gline
    $  indentBy (mkParentsNum + 3)
    <> "new "
    <> ocamlName
    <> " ("
    <> name n
    <> ".create pl))"
    <> packShowLabels parents
    <> closedParentheses makeParamsParents
 where
  isMakeParamsParent :: Text -> Name -> Bool
  isMakeParamsParent _ (Name "GObject" "Object") = False
  isMakeParamsParent currNS (Name ns _) | currNS /= ns = False
                                        | otherwise    = True
  makeParamsCont :: Name -> Int -> Text
  makeParamsCont parent 0 = indentBy 1 <> makeParams parent <> " [] ~cont:("
  makeParamsCont parent idx =
    indentBy (idx + 1) <> "fun pl -> " <> makeParams parent <> " pl ~cont:("

  makeParams :: Name -> Text
  makeParams (Name "Gtk" "Widget") = "GtkBase.Widget.size_params"
  makeParams (Name _     nm      ) = nm <> ".make_params"

  packShowLabels :: [Name] -> Text
  packShowLabels parents
    | Name "Gtk" "Widget" `elem` parents = " ~packing ~show"
    | otherwise                          = ""

  indentBy :: Int -> Text
  indentBy idx = T.replicate idx "  "

  closedParentheses :: [Name] -> Text
  closedParentheses makeParents = T.replicate (length makeParents + 1) ")"

genInterface :: Name -> Interface -> CodeGen ()
genInterface n iface = unless (n `elem` excludeFiles) $ do
  let name'     = upperName n
      ocamlName = escapeOCamlReserved $ camelCaseToSnakeCase (name n)

  -- addSectionDocumentation ToplevelSection (ifDocumentation iface)

  addType n Nothing

  case (ifCType iface, getIfCheckMacro iface) of
    (Just ctype, Just checkMacro) -> do
      genGObjectCasts n ctype checkMacro
      genMlTypeInit n
      genCInterfaceTypeInit iface n
    _ -> return ()

  when (namespace n == "Gtk") $ do
    line "open Gobject"
    line "open Data"
    blank

    gline "open Gobject"
    gblank
    gline $ "class " <> ocamlName <> "_skel obj = object (self)"
    gline
      $  "  method as_"
      <> ocamlName
      <> " = (obj :> "
      <> nsOCamlType (namespace n) n
      <> " obj)"
    gblank

    isGO <- apiIsGObject n (APIInterface iface)

    when isGO $ do
      -- Properties
      group $ genInterfaceProperties n iface

      -- Signals
      unless (null $ ifSignals iface) $ group $ do
        line "module S = struct"
        indent $ line "open GtkSignal"
        indent $ forM_ (ifSignals iface) $ \s -> handleCGExc
          ( commentLine
          . (T.concat
              [ "Could not generate signal "
              , name'
              , "::"
              , sigName s
              , " *)\n"
              , "(* Error was : "
              ] <>
            )
          . describeCGError
          )
          (genSignal s n)
        line "end"

    -- Methods
    let propNames = hyphensToUnderscores . propName <$> ifProperties iface
        -- TODO: use a Set
        getsSets  = (("get_" <>) <$> propNames) ++ (("set_" <>) <$> propNames)

    group $ forM_ (ifMethods iface) $ \m -> do
      let mn = methodName m

      unless (ocamlIdentifier mn `elem` getsSets) $ handleCGExc
        (\e -> line
          (  "(* Could not generate method "
          <> name'
          <> "::"
          <> name mn
          <> " *)\n"
          <> "(* Error was : "
          <> describeCGError e
          <> " *)"
          )
        )
        (genMethod n m)

    gline "end"
    gblank

    -- Generate the signal class only for GObjects
    when isGO $ do
      gline $ "and " <> ocamlName <> "_signals obj = object (self)"
      gline $ "  inherit [_] GObj.gobject_signals obj"
      forM_ (ifSignals iface) $ \s -> genGSignal s n
      gline "end"
      gblank

    gline $ "and " <> ocamlName <> " obj = object (self)"
    gline $ "  inherit " <> ocamlName <> "_skel obj"
    when isGO $ gline $ "  method connect = new " <> ocamlName <> "_signals obj"
    gline "end"
    gblank
