{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.WithConstantFields where

import Data.Aeson
import Data.Aeson.Deriving.Generic
import Data.Kind (Type)
import Data.Text (pack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Generics
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Control.Monad

-- | Add arbitrary constant fields to the encoded object
newtype WithConstantFields (obj :: k) (a :: Type) = WithConstantFields a
  deriving stock (Generic)

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

instance (ToJSON a, LoopWarning a, ToConstantObject obj) => ToJSON (WithConstantFields obj a) where
  toJSON (WithConstantFields x) = mapObjects (<> fields) $ toJSON x
    where
      fields = toConstantObject $ Proxy @obj

      mapObjects :: (Object -> Object) -> Value -> Value
      mapObjects f (Object o) = Object (f o)
      mapObjects _ val        = val

instance (FromJSON a, LoopWarning a, ToConstantObject obj) => FromJSON (WithConstantFields obj a) where
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
