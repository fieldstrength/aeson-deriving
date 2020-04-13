{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Aeson.Deriving.Utils
    ( mapObjects
    , mapField
    , All
    , textVal
    ) where

import Data.Aeson
import Data.Kind (Constraint)
import Data.Text
import qualified Data.HashMap.Strict as HashMap
import GHC.TypeLits
import Data.Proxy

mapObjects :: (Object -> Object) -> Value -> Value
mapObjects f (Object o) = Object (f o)
mapObjects _ val        = val

mapField :: Text -> (Value -> Value) -> Object -> Object
mapField str f = HashMap.mapWithKey $ \s x ->
  if s == str then f x else x

-- | Convenience constraint family. All @types@ in the list satisfy the @predicate@.
type family All (predicate :: k -> Constraint) (types :: [k]) :: Constraint where
  All predicate '[] = ()
  All predicate (t ': ts) = (predicate t, All predicate ts)


textVal :: KnownSymbol s => Proxy s -> Text
textVal = pack . symbolVal
