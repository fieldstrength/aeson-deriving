{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Deriving.Utils
    ( mapObjects
    , mapField
    , All
    , textVal
    ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Aeson
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KeyMap
import           Data.Kind              (Constraint)
import           Data.Proxy
import           GHC.TypeLits

mapObjects :: (Object -> Object) -> Value -> Value
mapObjects f (Object o) = Object (f o)
mapObjects _ val        = val

mapField :: Key -> (Value -> Value) -> Object -> Object
mapField str f = runIdentity . KeyMap.traverseWithKey
                 (\s x ->
                      pure $
                      if s == str then f x else x)

-- | Convenience constraint family. All @types@ in the list satisfy the @predicate@.
type family All (predicate :: k -> Constraint) (types :: [k]) :: Constraint where
  All predicate '[] = ()
  All predicate (t ': ts) = (predicate t, All predicate ts)


textVal :: KnownSymbol s => Proxy s -> Key.Key
textVal = Key.fromString . symbolVal
