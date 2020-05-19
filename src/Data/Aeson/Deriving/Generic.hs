{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.Generic
  ( -- * Typeclass for aeson 'Options'
    ToAesonOptions(..)
  -- * newtypes for Generic encodings
  -- ** Main data type for Generic encodings
  , GenericEncoded(..)
  -- ** Data type for encodings of composite "sum-of-records" types
  , RecordSumEncoded(..)
  -- * Phantom types for specifying Options
  -- ** Many-parameter type for explicitly providing all 'Options' fields.
  , GenericOptions
  -- ** Types for supplying specific Options fields
  -- *** Type representing field assignment
  , (:=)
  -- *** Typeclass for Options fields
  , ToAesonOptionsField
  -- *** Types representing Options fields
  , FieldLabelModifier
  , ConstructorTagModifier
  , AllNullaryToStringTag
  , OmitNothingFields
  , SumEncoding  -- technically an aeson reexport. Shouldn't matter.
  , UnwrapUnaryRecords
  , TagSingleConstructors
  --  *** String Functions
  , StringFunction(..)
  , SnakeCase
  , Uppercase
  , Lowercase
  , DropLowercasePrefix
  , DropPrefix
  , DropSuffix
  , Id
  , snakeCase
  , dropLowercasePrefix
  --  *** Sum encoding options
  , ToSumEncoding
  , UntaggedValue
  , ObjectWithSingleField
  , TwoElemArray
  , TaggedObject
  -- * Safety class
  , LoopWarning
  , DisableLoopWarning(..)
  -- * Convenience newtype
  , type (&) (Ampersand)
  , unAmpersand
  ) where

import           Data.Aeson
import           Data.Aeson.Deriving.Internal.Generic
import           Data.Aeson.Deriving.Known
