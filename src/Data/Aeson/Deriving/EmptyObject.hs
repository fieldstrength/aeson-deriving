{-# LANGUAGE UndecidableInstances #-}

-- | Allow unit-like types to be serialized as the empty object
--   This can be combined with 'WithConstantFields'.
module Data.Aeson.Deriving.EmptyObject where

import Data.Aeson
import Data.Kind (Type)
import GHC.Generics

-- | For data types with exactly one value, this data type changes the serialization to be the empty JSON object.
--   It can be combined with 'WithConstantFields'.
newtype EmptyObject a = EmptyObject a

instance UnitLike (Rep a) => ToJSON (EmptyObject a) where
  toJSON _ = object []

instance (Generic a, UnitLike (Rep a)) => FromJSON (EmptyObject a) where
  parseJSON = withObject "object" $ \_hashmap ->
    pure . EmptyObject $ to gPoint


-- | class for data types with a single constructor
class UnitLike (f :: Type -> Type) where
  gPoint :: f a

instance UnitLike U1 where
  gPoint = U1

instance UnitLike a => UnitLike (M1 C meta a) where
  gPoint = M1 gPoint

instance UnitLike a => UnitLike (M1 D meta a) where
  gPoint = M1 gPoint


