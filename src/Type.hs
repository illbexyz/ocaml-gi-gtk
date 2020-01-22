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
  , typeConName
  , varsInTypeRep
  , showTypeVar
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
  , typevar
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
-- since they have special syntax in their OCaml representation.
data TypeCon = TupleCon
             | ListCon
             | OptionCon
             | PolyCon PolyDirection
             | ObjCon
             | TextualCon Text
             | TypeVar Text Bool
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
typeShow (TypeRep ObjCon          args) = T.concat (map typeShow args) <> " obj"
typeShow (TypeRep (TypeVar var True) args) =
  "(" <> T.concat (map typeShow args) <> " as '" <> var <> ")"
typeShow (TypeRep (TypeVar var False) args) = T.concat (map typeShow args)
typeShow (TypeRep (TextualCon con   ) args) = T.intercalate
  " "
  (con : map (parenthesize . typeShow) args)
 where
  parenthesize :: Text -> Text
  parenthesize s = if T.any (== ' ') s then "(" <> s <> ")" else s

varsInTypeRep :: TypeRep -> [Text]
varsInTypeRep = inner []
 where
  inner acc (TypeRep (TypeVar var _) []) = var : acc
  inner acc (TypeRep (TypeVar var _) xs) = concatMap (inner (var : acc)) xs
  inner acc (TypeRep _               []) = acc
  inner acc (TypeRep _               xs) = concatMap (inner acc) xs

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
typeConName (TypeRep (TypeVar _ _ )  _) = "typevar"
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

conOk :: TypeCon -> [TypeRep] -> TypeRep
conOk TupleCon       xs = TypeRep { typeCon = TupleCon, typeConArgs = xs }
conOk ListCon        xs = TypeRep { typeCon = ListCon, typeConArgs = xs }
conOk OptionCon      xs = TypeRep { typeCon = OptionCon, typeConArgs = xs }
conOk (PolyCon dir)  xs = TypeRep { typeCon = PolyCon dir, typeConArgs = xs }
conOk ObjCon         xs = TypeRep { typeCon = ObjCon, typeConArgs = xs }
conOk (TypeVar t b ) xs = TypeRep { typeCon = TypeVar t b, typeConArgs = xs }
conOk (TextualCon t) xs = TypeRep { typeCon = TextualCon t, typeConArgs = xs }

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

typevar :: Text -> TypeRep -> TypeRep
typevar var t = TypeVar var False `conOk` [t]

showTypeVar :: TypeRep -> TypeRep
showTypeVar (TypeRep (TypeVar var _) []) = TypeRep (TypeVar var True) []
showTypeVar (TypeRep (TypeVar var _) xs) =
  TypeRep (TypeVar var True) (map showTypeVar xs)
showTypeVar t@(TypeRep _   []) = t
showTypeVar (  TypeRep con xs) = TypeRep con (map showTypeVar xs)
