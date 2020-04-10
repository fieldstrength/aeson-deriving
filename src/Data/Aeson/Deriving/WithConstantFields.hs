{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.WithConstantFields where

import Data.Aeson
import Data.Aeson.Deriving.Internal.Utils
import Data.Aeson.Deriving.Generic
import Data.Kind (Type)
import Data.Text (pack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Generics
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Control.Monad

-- | Add arbitrary constant fields to the encoded object and require them when decoding.
newtype WithConstantFields (obj :: k) (a :: Type) = WithConstantFields a
  deriving stock (Generic)

-- | Add arbitrary constant fields to the encoded object, but do not require them when
--   decoding.
newtype WithConstantFieldsOut (obj :: k) (a :: Type) = WithConstantFieldsOut a
  deriving stock (Generic)
  deriving ToJSON via (WithConstantFields obj a)
  deriving FromJSON via a

-- | Require arbitrary constant fields when decoding the object, but do not add them when
--   encoding.
newtype WithConstantFieldsIn (obj :: k) (a :: Type) = WithConstantFieldsIn a
  deriving stock (Generic)
  deriving ToJSON via a
  deriving FromJSON via (WithConstantFields obj a)


class ToConstant (a :: k) where
  toConstant :: Proxy a -> Value

class ToConstantObject (a :: k) where
  toConstantObject :: Proxy a -> Object

instance KnownSymbol str => ToConstant (str :: Symbol) where
  toConstant = String . pack . symbolVal

instance ToConstantObject '[] where
  toConstantObject _ = mempty

instance (ToConstantObject fields, KnownSymbol key, ToConstant val)
  => ToConstantObject ((key := val) ': fields) where
    toConstantObject _ =
      HashMap.insert
        (pack . symbolVal $ Proxy @key)
        (toConstant $ Proxy @val)
        (toConstantObject $ Proxy @fields)

instance (ToJSON a, LoopWarning (WithConstantFields obj) a, ToConstantObject obj) =>
  ToJSON (WithConstantFields obj a) where
    toJSON (WithConstantFields x) = mapObjects (<> fields) $ toJSON x
      where
        fields = toConstantObject $ Proxy @obj

instance (FromJSON a, LoopWarning (WithConstantFields obj) a, ToConstantObject obj) => FromJSON (WithConstantFields obj a) where
  parseJSON valIn = WithConstantFields <$>
    parseJSON valIn <*
      HashMap.traverseWithKey assertFieldPresent (toConstantObject $ Proxy @obj)

    where
      assertFieldPresent key valExpected =
        flip (withObject "Object") valIn $ \obj -> do
          valActual <- obj .: key
          unless (valActual == valExpected) . fail $
            "Expected constant value `" <> show valExpected <> "` but got: " <>
            show valActual

-- | Represents a function that maps the first value to the second,
--   and otherwise does nothing but return the input.
data a ==> b

class Function (a :: Type) where
  function :: Proxy a -> Value -> Value

instance All ToConstant [a, b] => Function (a ==> b) where
  function _ x
    | x == toConstant (Proxy @a) = toConstant (Proxy @b)
    | otherwise = x

instance All KnownSymbol [a, b] => StringFunction (a ==> b) where
  stringFunction _ x
    | x == symbolVal (Proxy @a) = symbolVal (Proxy @b)
    | otherwise = x

-- | Function that turns null supplies a default value for a
data WithDefault (val :: k)

instance ToConstant a => Function (WithDefault a) where
  function Proxy = \case
    Null -> toConstant $ Proxy @a
    x    -> x
