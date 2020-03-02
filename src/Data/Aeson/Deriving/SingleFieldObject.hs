{-# language UndecidableInstances #-}

module Data.Aeson.Deriving.SingleFieldObject where

import Data.Aeson
import Data.Aeson.Deriving.Generic (LoopWarning)
import Data.Text (pack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Generics
import Data.Proxy

-- | Puts the entire output of encoding the inner type within a single field
newtype SingleFieldObject (fieldName :: Symbol) a = SingleFieldObject { unSingleFieldObject :: a }
  deriving stock (Generic)

instance (ToJSON a, LoopWarning a, KnownSymbol fieldName) => ToJSON (SingleFieldObject fieldName a) where
  toJSON a = object
    [ ( pack . symbolVal $ Proxy @fieldName
      , toJSON . unSingleFieldObject $ a
      )
    ]

instance (FromJSON a, LoopWarning a, KnownSymbol fieldName) => FromJSON (SingleFieldObject fieldName a) where
  parseJSON = withObject "Object" $ \hm ->
    SingleFieldObject <$> hm .: (pack . symbolVal $ Proxy @fieldName)
