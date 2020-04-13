{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.Values where

import Data.Aeson
import Data.Aeson.Deriving.Utils
import Data.Aeson.Deriving.Generic
import Data.Kind (Type)
import Data.Text (pack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy


class ToConstant (a :: k) where
  toConstant :: Proxy a -> Value

instance KnownSymbol str => ToConstant (str :: Symbol) where
  toConstant = String . pack . symbolVal

instance ToConstant '[] where
  toConstant Proxy = toJSON @[Value] []

data Null

instance ToConstant Null where
  toConstant Proxy = Null

-- | Constant JSON lists
class ToConstantList (xs :: [k]) where
  toConstantList :: Proxy xs -> [Value]

instance ToConstantList '[] where
  toConstantList Proxy = []

instance (ToConstant x, ToConstantList xs) => ToConstantList (x ': xs) where
  toConstantList Proxy = toConstant (Proxy @x) : toConstantList (Proxy @xs)

instance ToConstantList (xs :: [k]) => ToConstant xs where
  toConstant Proxy = toJSON $ toConstantList (Proxy @xs)


-- | Constant JSON objects
class ToConstantObject (a :: k) where
  toConstantObject :: Proxy a -> Object

instance ToConstantObject '[] where
  toConstantObject _ = mempty

instance (ToConstantObject fields, KnownSymbol key, ToConstant val)
  => ToConstantObject ((key := val) ': fields) where
    toConstantObject _ =
      HashMap.insert
        (pack . symbolVal $ Proxy @key)
        (toConstant $ Proxy @val)
        (toConstantObject $ Proxy @fields)

-- | JSON 'Value' functions
class Function (a :: Type) where
  function :: Proxy a -> Value -> Value

-- | Represents a function that maps the first value to the second,
--   and otherwise does nothing but return the input.
data a ==> b

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
