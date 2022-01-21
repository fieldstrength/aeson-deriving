{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.SingleFieldObject where

import           Data.Aeson
import           Data.Aeson.Deriving.Generic (LoopWarning)
import qualified Data.Aeson.Key              as Key
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits                (KnownSymbol, Symbol, symbolVal)

-- | Puts the entire output of encoding the inner type within a single field
newtype SingleFieldObject (fieldName :: Symbol) a = SingleFieldObject a
  deriving stock (Generic)

instance (ToJSON a, LoopWarning (SingleFieldObject fieldName) a, KnownSymbol fieldName) =>
  ToJSON (SingleFieldObject fieldName a) where
    toJSON (SingleFieldObject a) = object
      [ ( Key.fromString . symbolVal $ Proxy @fieldName
        , toJSON a
        )
      ]

instance (FromJSON a, LoopWarning (SingleFieldObject fieldName) a, KnownSymbol fieldName) => FromJSON (SingleFieldObject fieldName a) where
  parseJSON = withObject "Object" $ \hm ->
    SingleFieldObject <$> hm .: (Key.fromString . symbolVal $ Proxy @fieldName)
