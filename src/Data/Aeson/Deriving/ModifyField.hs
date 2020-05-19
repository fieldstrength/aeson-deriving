{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.ModifyField where

import           Data.Aeson
import           Data.Aeson.Deriving.Generic
import           Data.Aeson.Deriving.Known
import           Data.Aeson.Deriving.Utils
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits                (KnownSymbol, Symbol)

-- | Modify the contents of a particular field while decoding.
newtype ModifyFieldIn (fieldName :: Symbol) fun a = ModifyFieldIn a
  deriving stock Generic
  deriving ToJSON via a

instance
  ( FromJSON a
  , KnownSymbol fieldName
  , KnownJSONFunction fun
  , LoopWarning (ModifyFieldIn fieldName fun) a)
  => FromJSON (ModifyFieldIn fieldName fun a) where
    parseJSON
      = fmap ModifyFieldIn
      . parseJSON @a
      . mapObjects (mapField (textVal $ Proxy @fieldName) (functionVal $ Proxy @fun))

-- | Modify the contents of a particular field while encoding.
newtype ModifyFieldOut (fieldName :: Symbol) fun a = ModifyFieldOut a
  deriving stock Generic
  deriving FromJSON via a

instance
  ( ToJSON a
  , KnownSymbol fieldName
  , KnownJSONFunction fun
  , LoopWarning (ModifyFieldOut fieldName fun) a)
  => ToJSON (ModifyFieldOut fieldName fun a) where
    toJSON (ModifyFieldOut x)
      = mapObjects (mapField (textVal $ Proxy @fieldName) (functionVal $ Proxy @fun))
      $ toJSON x

newtype RemapTextField fieldName haskVal jsonVal a = RemapTextField
  (ModifyFieldOut fieldName (haskVal ==> jsonVal)
    (ModifyFieldIn fieldName (jsonVal ==> haskVal) a))
  deriving newtype (FromJSON, ToJSON)
