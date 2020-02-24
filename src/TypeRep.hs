module TypeRep
  ( TypeRep(..)
  , PolyDirection(..)
  , typeShow
  , methodTypeShow
  , getVars
  , isOptional
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           API                            ( Name(..) )
import           Naming                         ( nsOCamlType )

data PolyDirection = Less | More
  deriving (Show, Eq)

data TypeRep = ListCon TypeRep
             | OptionCon TypeRep
             | ObjCon TypeRep
             | RowCon PolyDirection TypeRep
             | TypeVarCon Text TypeRep
             | TupleCon [TypeRep]
             | PolyCon TypeRep
             | TextCon Text
             | NameCon Name
  deriving (Show, Eq)

typeShow :: Text -> TypeRep -> Text
typeShow currNS  (ListCon   t          ) = typeShow currNS t <> " list"
typeShow currNS  (OptionCon t          ) = typeShow currNS t <> " option"
typeShow currNS  (ObjCon    t          ) = typeShow currNS t <> " Gobject.obj"
typeShow currNS  (RowCon     Less  t   ) = "[< " <> typeShow currNS t <> "]"
typeShow currNS  (RowCon     More  t   ) = "[> " <> typeShow currNS t <> "]"
typeShow currNS  (TypeVarCon _tvar t   ) = typeShow currNS t
typeShow currNS  (PolyCon t@(NameCon _)) = typeShow currNS t
typeShow currNS  (PolyCon t            ) = "`" <> typeShow currNS t
typeShow currNS  (NameCon n            ) = nsOCamlType currNS n
typeShow _currNS (TextCon text         ) = text
typeShow currNS (TupleCon treps) =
  "(" <> T.intercalate " * " (typeShow currNS <$> treps) <> ")"


methodTypeShow :: Text -> TypeRep -> Text
methodTypeShow currNS (ListCon   t  ) = methodTypeShow currNS t <> " list"
methodTypeShow currNS (OptionCon t  ) = methodTypeShow currNS t <> " option"
methodTypeShow currNS (ObjCon t) = methodTypeShow currNS t <> " Gobject.obj"
methodTypeShow currNS (RowCon Less t) = "[< " <> methodTypeShow currNS t <> "]"
methodTypeShow currNS (RowCon More t) = "[> " <> methodTypeShow currNS t <> "]"
methodTypeShow currNS (TypeVarCon var t) =
  "(" <> methodTypeShow currNS t <> " as '" <> var <> ")"
methodTypeShow currNS  (PolyCon t@(NameCon _)) = methodTypeShow currNS t
methodTypeShow currNS  (PolyCon t            ) = "`" <> methodTypeShow currNS t
methodTypeShow currNS  (NameCon n            ) = nsOCamlType currNS n
methodTypeShow _currNS (TextCon text         ) = text
methodTypeShow currNS (TupleCon treps) =
  "(" <> T.intercalate " * " (methodTypeShow currNS <$> treps) <> ")"

getVars :: Text -> TypeRep -> [Text]
getVars currNS (OptionCon (ObjCon (TypeVarCon _tvar (RowCon Less (PolyCon (NameCon (Name ns _)))))))
  | ns == currNS
  = []
getVars currNS (ObjCon (TypeVarCon _tvar (RowCon Less (PolyCon (NameCon (Name ns _))))))
  | ns == currNS
  = []
getVars currNS  (TypeVarCon var t) = var : getVars currNS t
getVars currNS  (ListCon   t     ) = getVars currNS t
getVars currNS  (OptionCon t     ) = getVars currNS t
getVars currNS  (ObjCon    t     ) = getVars currNS t
getVars currNS  (RowCon Less t   ) = getVars currNS t
getVars currNS  (RowCon More t   ) = getVars currNS t
getVars currNS  (PolyCon  t      ) = getVars currNS t
getVars _currNS (NameCon  _      ) = []
getVars _currNS (TextCon  _      ) = []
getVars currNS  (TupleCon treps  ) = concatMap (getVars currNS) treps

isOptional :: TypeRep -> Bool
isOptional (OptionCon _) = True
isOptional _             = False
