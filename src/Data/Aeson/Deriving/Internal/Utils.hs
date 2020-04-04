{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.Internal.Utils (mapObjects) where

import Data.Aeson

mapObjects :: (Object -> Object) -> Value -> Value
mapObjects f (Object o) = Object (f o)
mapObjects _ val        = val
