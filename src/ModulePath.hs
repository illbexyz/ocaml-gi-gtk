{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Abstract representation for paths into modules.
module ModulePath
    ( ModulePath(..)
    , toModulePath
    , (/.)
    , dotModulePath
    , addNamePrefix
    )
where

import           Data.Monoid                    ( Monoid(..)
                                                , (<>)
                                                )
import           Data.String                    ( IsString(..) )
import qualified Data.Text                     as T
import qualified Data.Semigroup                as Sem
import           Data.Text                      ( Text )

import           Util                           ( ucFirst
                                                , mapNth
                                                )

-- | A path to a module.
newtype ModulePath = ModulePath { modulePathToList :: [Text] }
  deriving (Sem.Semigroup, Monoid, Eq, Show, Ord)

-- | Construct a `ModulePath` from a `String`.
instance IsString ModulePath where
    fromString = toModulePath . T.pack

-- | Construct a path into the given GIR namespace. The given `Text`
-- will be split along ".".
--
-- === __Examples__
-- >>> dotModulePath (toModulePath "Foo")
-- "Foo"
--
-- >>> dotModulePath ("Foo" <> toModulePath "Bar.Baz")
-- "Foo.Bar.Baz"
--
-- >>> dotModulePath ("Foo" <> toModulePath "bar.baz")
-- "Foo.Bar.Baz"
toModulePath :: Text -> ModulePath
toModulePath p = ModulePath (map ucFirst (T.split (== '.') p))

-- | Turn a module path into the corresponding dotted string. Note
-- that the implementation ensures that the module names start with a
-- capital letter.
--
-- === __Examples__
-- >>> dotModulePath ("Foo" /. "Bar" /. "Baz")
-- "Foo.Bar.Baz"
--
-- >>> dotModulePath ("foo" /. "bar" /. "baz")
-- "Foo.Bar.Baz"
dotModulePath :: ModulePath -> Text
dotModulePath (ModulePath mp) = T.intercalate "." mp

-- | Append the given component to the given module path.
--
-- === __Examples__
-- >>> dotModulePath ("Foo" /. "Bar")
-- "Foo.Bar"
(/.) :: ModulePath -> Text -> ModulePath
(/.) mp p = mp <> toModulePath p

addNamePrefix :: Text -> ModulePath -> ModulePath
addNamePrefix prefix mp =
    let pathList = modulePathToList mp
    in  ModulePath $ mapNth (length pathList - 1) (prefix <>) pathList
