{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.Known where

import Data.Aeson
import Data.Aeson.Deriving.Utils
import Data.Kind (Type)
import Data.Text (pack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy


class KnownJSON (a :: k) where
  jsonVal :: Proxy a -> Value

instance KnownSymbol str => KnownJSON (str :: Symbol) where
  jsonVal = String . pack . symbolVal

instance KnownBool b => KnownJSON (b :: Bool) where
  jsonVal = Bool . boolVal

instance KnownJSON '[] where
  jsonVal Proxy = toJSON @[Value] []

data Null

instance KnownJSON Null where
  jsonVal Proxy = Null


class KnownBool (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance KnownBool 'True where boolVal Proxy = True
instance KnownBool 'False where boolVal Proxy = False


-- | Constant JSON lists
class KnownJSONList (xs :: [k]) where
  listVal :: Proxy xs -> [Value]

instance KnownJSONList '[] where
  listVal Proxy = []

instance (KnownJSON x, KnownJSONList xs) => KnownJSONList (x ': xs) where
  listVal Proxy = jsonVal (Proxy @x) : listVal (Proxy @xs)

instance KnownJSONList (xs :: [k]) => KnownJSON xs where
  jsonVal Proxy = toJSON $ listVal (Proxy @xs)


-- | Phantom data type to make explicit which fields we pass for Aeson options. Polykinded
--   in the second argument so it can take i.e. Booleans or Symbols where needed.
--
--   Also used for specifying constant values added to, or required from, an encoding.
--   See "Data.Aeson.Deriving.WithConstantFields".
data field := (value :: k)

-- | Constant JSON objects
class KnownJSONObject (a :: k) where
  objectVal :: Proxy a -> Object

instance KnownJSONObject '[] where
  objectVal Proxy = mempty

instance (KnownJSONObject fields, KnownSymbol key, KnownJSON val)
  => KnownJSONObject ((key := val) ': fields) where
    objectVal Proxy =
      HashMap.insert
        (pack . symbolVal $ Proxy @key)
        (jsonVal $ Proxy @val)
        (objectVal $ Proxy @fields)

-- | JSON 'Value' functions
class KnownJSONFunction (a :: Type) where
  functionVal :: Proxy a -> Value -> Value

-- | Represents a function that maps the first value to the second,
--   and otherwise does nothing but return the input.
data a ==> b

instance All KnownJSON [a, b] => KnownJSONFunction (a ==> b) where
  functionVal Proxy x
    | x == jsonVal (Proxy @a) = jsonVal (Proxy @b)
    | otherwise = x

-- | Function that turns nulls into the given default value
data WithDefault (val :: k)

instance KnownJSON a => KnownJSONFunction (WithDefault a) where
  functionVal Proxy = \case
    Null -> jsonVal $ Proxy @a
    x    -> x
