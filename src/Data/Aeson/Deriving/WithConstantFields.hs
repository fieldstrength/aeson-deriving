{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.WithConstantFields where

import           Control.Monad               (unless)
import           Data.Aeson
import           Data.Aeson.Deriving.Generic
import           Data.Aeson.Deriving.Known
import           Data.Aeson.Deriving.Utils
import qualified Data.Aeson.KeyMap           as KeyMap
import           Data.Kind                   (Type)
import           Data.Proxy
import           GHC.Generics

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


instance (ToJSON a, LoopWarning (WithConstantFields obj) a, KnownJSONObject obj) =>
  ToJSON (WithConstantFields obj a) where
    toJSON (WithConstantFields x) = mapObjects (<> fields) $ toJSON x
      where
        fields = objectVal $ Proxy @obj

instance (FromJSON a, LoopWarning (WithConstantFields obj) a, KnownJSONObject obj) => FromJSON (WithConstantFields obj a) where
  parseJSON valIn = WithConstantFields <$>
    parseJSON valIn <*
      KeyMap.traverseWithKey assertFieldPresent (objectVal $ Proxy @obj)

    where
      assertFieldPresent key valExpected =
        flip (withObject "Object") valIn $ \obj -> do
          valActual <- obj .: key
          unless (valActual == valExpected) . fail $
            "Expected constant value " <> showEscapedJson valExpected <>
            " but got: " <> showEscapedJson valActual

      showEscapedJson val = show (encode val)
