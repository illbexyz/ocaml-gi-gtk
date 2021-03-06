module TypeRep
  ( TypeRep(..)
  , RowDirection(..)
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

data RowDirection = Less | More
  deriving (Show, Eq)

data TypeRep = ListCon TypeRep              -- list type constructor
             | OptionCon TypeRep            -- option type constructor
             | ObjCon TypeRep               -- Gobject.obj type constructor
             | RowCon RowDirection TypeRep  -- [> ] and [< ] type constructor
             | TypeVarCon Text TypeRep      -- adds a type variable ('a, 'b, ...)
             | TupleCon [TypeRep]           -- tuple type constructor ( * * )
             | PolyCon TypeRep              -- To represent a polymorphic variant constructor
             | TextCon Text                 -- For atomic types
             | NameCon Name                 -- For object/interfaces

typeShow :: Text -> TypeRep -> Text
typeShow currNS (ListCon   t          ) = typeShow currNS t <> " list"
typeShow currNS (OptionCon t          ) = typeShow currNS t <> " option"
typeShow currNS (ObjCon    t          ) = typeShow currNS t <> " Gobject.obj"
typeShow currNS (RowCon     Less t    ) = "[< " <> typeShow currNS t <> "]"
typeShow currNS (RowCon     More t    ) = "[> " <> typeShow currNS t <> "]"
typeShow currNS (TypeVarCon var  t    ) = typeShow currNS t
typeShow currNS (PolyCon t@(NameCon n)) = typeShow currNS t
typeShow currNS (PolyCon t            ) = "`" <> typeShow currNS t
typeShow currNS (NameCon n            ) = nsOCamlType currNS n
typeShow currNS (TextCon text         ) = text
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
methodTypeShow currNS (PolyCon t@(NameCon n)) = methodTypeShow currNS t
methodTypeShow currNS (PolyCon t            ) = "`" <> methodTypeShow currNS t
methodTypeShow currNS (NameCon n            ) = nsOCamlType currNS n
methodTypeShow currNS (TextCon text         ) = text
methodTypeShow currNS (TupleCon treps) =
  "(" <> T.intercalate " * " (methodTypeShow currNS <$> treps) <> ")"

getVars :: TypeRep -> [Text]
getVars (OptionCon (ObjCon (TypeVarCon tvar (RowCon Less (PolyCon (NameCon n@(Name "Gtk" _)))))))
  = []
getVars (ObjCon (TypeVarCon tvar (RowCon Less (PolyCon (NameCon n@(Name "Gtk" _))))))
  = []
getVars (TypeVarCon var t) = var : getVars t
getVars (ListCon   t     ) = getVars t
getVars (OptionCon t     ) = getVars t
getVars (ObjCon    t     ) = getVars t
getVars (RowCon Less t   ) = getVars t
getVars (RowCon More t   ) = getVars t
getVars (PolyCon  t      ) = getVars t
getVars (NameCon  n      ) = []
getVars (TextCon  text   ) = []
getVars (TupleCon treps  ) = concatMap getVars treps

isOptional :: TypeRep -> Bool
isOptional (OptionCon _) = True
isOptional _             = False
