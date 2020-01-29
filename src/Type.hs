-- | An abstraction for representing type constructors. This is a very
-- simplified version of `Data.Typeable`, which we don't use directly
-- to avoid compatibility headaches.
module Type
  ( Type(..)  -- Reexported for convenience.
  , BasicType(..)
  , TypeRep(..)
  , TypeCon(..)
  , PolyDirection(..)
  , con
  , con0
  , typeShow
  , typeShowWithClass
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
  , list
  , typevar
  , classCon
  , isHighLevelObj
  , isOptional
  , getTextualCons
  , mapPolyToClass
  , containsClass
  , extractClass
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Data.GI.GIR.BasicTypes         ( Type(..)
                                                , BasicType(..)
                                                , Name(..)
                                                )

-- | A fully applied type.
data TypeRep = TypeRep { typeCon     :: TypeCon
                       , typeConArgs :: [TypeRep]
                       } deriving (Show, Eq)

data PolyDirection = Less
                   | More
                   | Exact
  deriving (Show, Eq)

-- | A type constructor. We single out some specific constructors
-- since they have special syntax in their OCaml representation.
data TypeCon = TupleCon
             | ListCon
             | OptionCon
             | PolyCon PolyDirection
             | ObjCon
             | TextualCon Text
             | TypeVar Text Bool
             | ClassCon Text
  deriving (Show, Eq)

-- | Give a valid Haskell source representation of the given
-- `TypeRep`.
typeShow'
  :: Bool                   -- ^ show class
  -> TypeRep
  -> Text
typeShow' sc (TypeRep TupleCon args) =
  "(" <> T.intercalate " * " (map (typeShow' sc) args) <> ")"
typeShow' sc (TypeRep ListCon args) =
  T.intercalate ", " (map (typeShow' sc) args) <> " list"
typeShow' sc (TypeRep OptionCon args) =
  T.concat (map (typeShow' sc) args) <> " option"
typeShow' sc (TypeRep (PolyCon More) args) =
  "[> " <> T.intercalate ", " (map (typeShow' sc) args) <> "]"
typeShow' sc (TypeRep (PolyCon Less) args) =
  "[< " <> T.intercalate ", " (map (typeShow' sc) args) <> "]"
typeShow' sc (TypeRep (PolyCon Exact) args) =
  T.concat (map (typeShow' sc) args)
typeShow' sc (TypeRep ObjCon args) =
  T.concat (map (typeShow' sc) args) <> " obj"
typeShow' sc (TypeRep (TypeVar var True) args) =
  "(" <> T.concat (map (typeShow' sc) args) <> " as '" <> var <> ")"
typeShow' sc (TypeRep (TypeVar var False) args) =
  T.concat (map (typeShow' sc) args)
typeShow' False (TypeRep (ClassCon n) args) =
  "`" <> last (T.splitOn "." n) <> T.concat (map (typeShow' False) args)
typeShow' True (TypeRep (ClassCon t) args) =
  "#" <> t <> "_o" <> T.concat (map (typeShow' False) args)
typeShow' sc (TypeRep (TextualCon con) args) = T.intercalate
  " "
  (con : map (parenthesize . (typeShow' sc)) args)
 where
  parenthesize :: Text -> Text
  parenthesize s = if T.any (== ' ') s then "(" <> s <> ")" else s

typeShow :: TypeRep -> Text
typeShow = typeShow' False

typeShowWithClass :: TypeRep -> Text
typeShowWithClass = typeShow' True

varsInTypeRep :: TypeRep -> [Text]
varsInTypeRep = inner []
 where
  inner acc (TypeRep (TypeVar var _) []) = var : acc
  inner acc (TypeRep (TypeVar var _) xs) = concatMap (inner (var : acc)) xs
  inner acc (TypeRep _               []) = acc
  inner acc (TypeRep _               xs) = concatMap (inner acc) xs

mapPolyToClass :: TypeRep -> TypeRep
mapPolyToClass t = if containsClass t then mapPolyToClass' t else t
 where
  mapPolyToClass' (TypeRep (TextualCon t) args) | "`" `T.isPrefixOf` t =
    TypeRep (TextualCon (T.tail t)) (map mapPolyToClass' args)
  mapPolyToClass' (TypeRep ObjCon []) = TypeRep (TextualCon "") []
  mapPolyToClass' (TypeRep ObjCon args) =
    TypeRep (TextualCon "") (map mapPolyToClass' args)
  mapPolyToClass' (TypeRep (PolyCon _) []) = TypeRep (TextualCon "") []
  mapPolyToClass' (TypeRep (PolyCon _) args) =
    TypeRep (TextualCon "") (map mapPolyToClass' args)
  mapPolyToClass' (TypeRep c args) = TypeRep c (map mapPolyToClass' args)

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
conOk TupleCon       xs = TypeRep TupleCon xs
conOk ListCon        xs = TypeRep ListCon xs
conOk OptionCon      xs = TypeRep OptionCon xs
conOk (PolyCon dir)  xs = TypeRep (PolyCon dir) xs
conOk ObjCon         xs = TypeRep ObjCon xs
conOk (TypeVar t b ) xs = TypeRep (TypeVar t b) xs
conOk (TextualCon t) xs = TypeRep (TextualCon t) xs
conOk (ClassCon   n) xs = TypeRep (ClassCon n) xs

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

list :: TypeRep -> TypeRep
list t = "[]" `con` [t]

typevar :: Text -> TypeRep -> TypeRep
typevar var t = TypeVar var False `conOk` [t]

classCon :: Text -> TypeRep
classCon t = ClassCon t `conOk` []

showTypeVar :: TypeRep -> TypeRep
showTypeVar (TypeRep (TypeVar var _) []) = TypeRep (TypeVar var True) []
showTypeVar (TypeRep (TypeVar var _) xs) =
  TypeRep (TypeVar var True) (map showTypeVar xs)
showTypeVar t@(TypeRep _   []) = t
showTypeVar (  TypeRep con xs) = TypeRep con (map showTypeVar xs)

isHighLevelObj :: TypeRep -> Bool
isHighLevelObj (TypeRep OptionCon [TypeRep ObjCon [TypeRep (PolyCon Less) [TypeRep (TextualCon _) []]]])
  = True
isHighLevelObj (TypeRep ObjCon [TypeRep (PolyCon Less) [TypeRep (TextualCon _) []]])
  = True
isHighLevelObj _ = False

isOptional :: TypeRep -> Bool
isOptional TypeRep { typeCon = OptionCon, typeConArgs = _ } = True
isOptional _ = False

getTextualCons :: TypeRep -> Text
getTextualCons TypeRep { typeCon = TextualCon t, typeConArgs = _ } = t
getTextualCons TypeRep { typeCon = _, typeConArgs = [] }           = ""
getTextualCons TypeRep { typeCon = _, typeConArgs = (a : as) } =
  getTextualCons a

containsClass :: TypeRep -> Bool
containsClass (TypeRep (ClassCon _) []  ) = True
containsClass (TypeRep _            []  ) = False
containsClass (TypeRep (ClassCon _) args) = True
containsClass (TypeRep _            args) = True `elem` map containsClass args

extractClass :: TypeRep -> Text
extractClass (TypeRep (ClassCon t) []  ) = last $ T.splitOn "." t
extractClass (TypeRep _            []  ) = ""
extractClass (TypeRep (ClassCon t) args) = last $ T.splitOn "." t
extractClass (TypeRep _            args) = T.concat $ map extractClass args
