{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.ModifyFields where

import Data.Aeson
import Data.Aeson.Deriving.Generic
import Data.Aeson.Deriving.Internal.Utils
import Data.Aeson.Deriving.WithConstantFields
import GHC.TypeLits (Symbol, KnownSymbol)
import GHC.Generics
import Data.Proxy

-- | Modify the contents of a particular field while decoding.
newtype ModifyFieldIn (fieldName :: Symbol) fun a = ModifyFieldIn a
  deriving stock Generic
  deriving ToJSON via a

instance
  ( FromJSON a
  , KnownSymbol fieldName
  , Function fun
  , LoopWarning (ModifyFieldIn fieldName fun) a)
  => FromJSON (ModifyFieldIn fieldName fun a) where
    parseJSON
      = fmap ModifyFieldIn
      . parseJSON @a
      . mapObjects (mapField (textVal $ Proxy @fieldName) (function $ Proxy @fun))

-- | Modify the contents of a particular field while encoding.
newtype ModifyFieldOut (fieldName :: Symbol) fun a = ModifyFieldOut a
  deriving stock Generic
  deriving FromJSON via a

instance
  ( ToJSON a
  , KnownSymbol fieldName
  , Function fun
  , LoopWarning (ModifyFieldOut fieldName fun) a)
  => ToJSON (ModifyFieldOut fieldName fun a) where
    toJSON (ModifyFieldOut x)
      = mapObjects (mapField (textVal $ Proxy @fieldName) (function $ Proxy @fun))
      $ toJSON x

type ModifyField fieldName haskVal jsonVal a =
  ModifyFieldOut fieldName (haskVal ==> jsonVal)
    (ModifyFieldIn fieldName (jsonVal ==> haskVal) a)
