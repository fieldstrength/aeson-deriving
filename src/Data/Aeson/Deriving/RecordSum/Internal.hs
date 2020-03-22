{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.RecordSum.Internal where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind (Type)
import           Data.Proxy
import           GHC.Generics
import           Data.Bifunctor (first)


newtype ParserMap a = ParserMap (HashMap String (Value -> Parser a))
  deriving stock Functor
  deriving newtype (Semigroup, Monoid)

unsafeMapKeys :: (String -> String) -> ParserMap a -> ParserMap a
unsafeMapKeys f (ParserMap hm)
  = ParserMap
  . HashMap.fromList
  . fmap (first f)
  $ HashMap.toList hm

-- | Provides a map from the (Haskell) constructor names of the inner contained types,
--   To parsers for the (Rep of the) outer data type that carries them.
class GTagParserMap (repA :: Type -> Type) where
  gParserMap :: Proxy repA -> ParserMap (repA x)

-- | We can create a ParserMap from any reference to a data type that has a
--   FromJSON instance (and at least one available constructor).
instance (GConstructorNames (Rep a), FromJSON a) => GTagParserMap (Rec0 a) where
  gParserMap _ = ParserMap . HashMap.fromList $ do
    constructorName <- gConstructorNames $ Proxy @(Rep a)
    [(constructorName, fmap K1 . parseJSON)]

-- | ParserMaps are trivially extended to the representation of fields under a constructor
instance GTagParserMap repA => GTagParserMap (S1 meta repA) where
  gParserMap _ = M1 <$> gParserMap (Proxy @repA)

-- | ParserMaps are extended to the canonical representation of a constructor.
--   Because there is no instance for :*:, this constraint is only satisfied for types
--   with a single constructor (with an S1 Directly under the C1 in the canonical rep).
instance GTagParserMap repA => GTagParserMap (C1 meta repA) where
  gParserMap _ = M1 <$> gParserMap (Proxy @repA)

-- | ParserMaps corresponding to different cases of a sum type are combined by merging.
instance (GTagParserMap repA, GTagParserMap repB) => GTagParserMap (repA :+: repB) where
  gParserMap _ =
       (L1 <$> gParserMap (Proxy @repA))
    <> (R1 <$> gParserMap (Proxy @repB))

-- | The ParserMap for the whole data type is now the one we get from under this D1 constructor.
instance GTagParserMap repA => GTagParserMap (D1 meta repA) where
  gParserMap _ = M1 <$> gParserMap (Proxy @repA)


-- | Provides constructor names
class GConstructorNames (repA :: Type -> Type) where
  gConstructorNames :: Proxy repA -> [String]

instance Constructor constructorMeta => GConstructorNames (C1 constructorMeta r) where
  gConstructorNames _ = [conName @constructorMeta undefined]

instance (GConstructorNames x, GConstructorNames y) => GConstructorNames (x :+: y) where
  gConstructorNames _ =
       gConstructorNames (Proxy @x)
    <> gConstructorNames (Proxy @y)

instance GConstructorNames r => GConstructorNames (D1 datatypeMeta r) where
  gConstructorNames _ = gConstructorNames $ Proxy @r
