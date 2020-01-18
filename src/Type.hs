-- | An abstraction for representing type constructors. This is a very
-- simplified version of `Data.Typeable`, which we don't use directly
-- to avoid compatibility headaches.
module Type
  ( Type(..)  -- Reexported for convenience.
  , BasicType(..)
  , TypeRep
  , con
  , con0
  , typeShow
  , typeShowPolyToAlpha
  , typeConName
  , io
  , ptr
  , funptr
  , maybeT
  , poly
  , polyMore
  , polyLess
  , obj
  , option
  , tuple
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Data.GI.GIR.BasicTypes         ( Type(..)
                                                , BasicType(..)
                                                )

-- | A fully applied type.
data TypeRep = TypeRep { typeCon     :: TypeCon
                       , typeConArgs :: [TypeRep]
                       } deriving (Eq)

data PolyDirection = Less
                   | More
                   | Exact
  deriving (Eq)

-- | A type constructor. We single out some specific constructors
-- since they have special syntax in their Haskell representation.
data TypeCon = TupleCon
             | ListCon
             | OptionCon
             | PolyCon PolyDirection
             | ObjCon
             | TextualCon Text
  deriving (Eq)

-- | Give a valid Haskell source representation of the given
-- `TypeRep`.
typeShow :: TypeRep -> Text
typeShow (TypeRep TupleCon args) =
  "(" <> T.intercalate " * " (map typeShow args) <> ")"
typeShow (TypeRep ListCon args) =
  "[" <> T.intercalate ", " (map typeShow args) <> "]"
typeShow (TypeRep OptionCon args) = T.concat (map typeShow args) <> " option"
typeShow (TypeRep (PolyCon More) args) =
  "[>`" <> T.intercalate ", " (map typeShow args) <> "]"
typeShow (TypeRep (PolyCon Less) args) =
  "[<`" <> T.intercalate ", " (map typeShow args) <> "]"
typeShow (TypeRep (PolyCon Exact) args) = "`" <> T.concat (map typeShow args)
typeShow (TypeRep ObjCon args) = T.concat (map typeShow args) <> " obj"
typeShow (TypeRep (TextualCon con) args) = T.intercalate
  " "
  (con : map (parenthesize . typeShow) args)
 where
  parenthesize :: Text -> Text
  parenthesize s = if T.any (== ' ') s then "(" <> s <> ")" else s

-- TODO: I don't like the repetition of the function
--       at all, investigate an alternative
typeShowPolyToAlpha :: TypeRep -> Text
typeShowPolyToAlpha (TypeRep TupleCon args) =
  "(" <> T.intercalate " * " (map typeShowPolyToAlpha args) <> ")"
typeShowPolyToAlpha (TypeRep ListCon args) =
  "[" <> T.intercalate ", " (map typeShowPolyToAlpha args) <> "]"
typeShowPolyToAlpha (TypeRep OptionCon args) =
  T.concat (map typeShowPolyToAlpha args) <> " option"
typeShowPolyToAlpha (TypeRep (PolyCon More) args) =
  "'a.([>`" <> T.intercalate ", " (map typeShowPolyToAlpha args) <> "] as 'a)"
typeShowPolyToAlpha (TypeRep (PolyCon Less) args) =
  "'a.([<`" <> T.intercalate ", " (map typeShowPolyToAlpha args) <> "] as 'a)"
typeShowPolyToAlpha (TypeRep (PolyCon Exact) args) =
  "`" <> T.concat (map typeShowPolyToAlpha args)
typeShowPolyToAlpha (TypeRep ObjCon args) =
  T.concat (map typeShowPolyToAlpha args) <> " obj"
typeShowPolyToAlpha (TypeRep (TextualCon con) args) = T.intercalate
  " "
  (con : map (parenthesize . typeShowPolyToAlpha) args)
 where
  parenthesize :: Text -> Text
  parenthesize s = if T.any (== ' ') s then "(" <> s <> ")" else s

-- | Return a textual representation of the type constructor for the
-- given `TypeRep`.
typeConName :: TypeRep -> Text
typeConName (TypeRep TupleCon        _) = "(,)"
typeConName (TypeRep ListCon         _) = "[,]"
typeConName (TypeRep OptionCon       _) = "option"
typeConName (TypeRep (PolyCon More ) _) = "[`>]"
typeConName (TypeRep (PolyCon Less ) _) = "[`<]"
typeConName (TypeRep (PolyCon Exact) _) = "`"
typeConName (TypeRep ObjCon          _) = "obj"
typeConName (TypeRep (TextualCon s)  _) = s

-- | Type constructor applied to the given types.
con :: Text -> [TypeRep] -> TypeRep
con "[]"     xs = TypeRep { typeCon = ListCon, typeConArgs = xs }
con "(,)"    xs = TypeRep { typeCon = TupleCon, typeConArgs = xs }
con "option" xs = TypeRep { typeCon = OptionCon, typeConArgs = xs }
con "[>`]"   xs = TypeRep { typeCon = PolyCon More, typeConArgs = xs }
con "[<`]"   xs = TypeRep { typeCon = PolyCon Less, typeConArgs = xs }
con "`"      xs = TypeRep { typeCon = PolyCon Exact, typeConArgs = xs }
con "obj"    xs = TypeRep { typeCon = ObjCon, typeConArgs = xs }
con s        xs = TypeRep { typeCon = TextualCon s, typeConArgs = xs }

-- | A shorthand for a type constructor taking no arguments.
con0 :: Text -> TypeRep
con0 c = con c []

-- | Embed in the `IO` monad.
io :: TypeRep -> TypeRep
io t = "IO" `con` [t]

-- | A `Ptr` to the type.
ptr :: TypeRep -> TypeRep
ptr t = "Ptr" `con` [t]

-- | A `FunPtr` to the type.
funptr :: TypeRep -> TypeRep
funptr t = "FunPtr" `con` [t]

-- | Embed in the `Maybe` monad.
maybeT :: TypeRep -> TypeRep
maybeT t = "option" `con` [t]

-- | Embed in the `Maybe` monad.
option :: TypeRep -> TypeRep
option t = "option" `con` [t]

poly :: TypeRep -> TypeRep
poly t = "`" `con` [t]

-- | Embed in a polymorphic variant
polyMore :: TypeRep -> TypeRep
polyMore t = "[>`]" `con` [t]

-- | Embed in a polymorphic variant
polyLess :: TypeRep -> TypeRep
polyLess t = "[<`]" `con` [t]

obj :: TypeRep -> TypeRep
obj t = "obj" `con` [t]

tuple :: [TypeRep] -> TypeRep
tuple t = "(,)" `con` t
