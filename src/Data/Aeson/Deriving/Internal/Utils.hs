{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Aeson.Deriving.Internal.Utils
    ( mapObjects
    , mapStrings
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

mapStrings :: (Text -> Text) -> Value -> Value
mapStrings f (String txt) = String (f txt)
mapStrings _ val          = val

mapField :: Text -> (Value -> Value) -> Object -> Object
mapField str f = HashMap.mapWithKey $ \s x ->
  if s == str then f x else x

type family All (c :: k -> Constraint) (cs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)


textVal :: KnownSymbol s => Proxy s -> Text
textVal = pack . symbolVal
