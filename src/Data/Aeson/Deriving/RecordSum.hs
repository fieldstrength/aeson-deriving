{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | Provides Generic functionality to track the parsers for different
--   unary constructors of a sum type, in order to improve error messages.
module Data.Aeson.Deriving.RecordSum
  ( RecordSumEncoded (..)
  ) where

import           Data.Aeson
import           Data.Aeson.Deriving.Generic
import           Data.Aeson.Deriving.RecordSum.Internal
import           Data.Aeson.Types (modifyFailure)
import           Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind (Type)
import           Data.List (intercalate)
import           Data.Proxy (Proxy(..))
import           Data.Text (pack)
import           GHC.Generics
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

-- | An encoding scheme for sums of records that are defined as distinct data types.
--   If we have a number of record types we want to combine under a sum, a straightforward
--   solution is to ensure that each each inner type uses a constructor tag, and then
--   derive the sum with @SumEncoding := UntaggedValue@. This works fine for the happy
--   path, but makes for very bad error messages, since it means that decoding proceeds by
--   trying each case in sequence. Thus error messages always pertain to the last type in
--   the sum, even when it wasn't the intended payload. This newtype improves on that
--   solution by providing the relevant error messages, by remembering the correspondence
--   between the constructor tag and the intended inner type/parser.
--
--   In order to work correctly, the inner types must use the 'TaggedObject' encoding.
--   The same tag field name and 'ConstructorTagModifier' must be supplied to this type.
newtype RecordSumEncoded (tagKey :: Symbol) (tagModifier :: k) (a :: Type) = RecordSumEncoded a

instance
  ( Generic a
  , GFromJSON Zero (Rep a)
  , GTagParserMap (Rep a)
  , Rep a ~ D1 meta cs
  , Datatype meta
  , StringFunction tagModifier
  , KnownSymbol tagKey)
    => FromJSON (RecordSumEncoded tagKey tagModifier a) where
      parseJSON val = prependErrMsg outerErrorMsg . flip (withObject "Object") val $ \hm -> do
        tagVal <- hm .: pack tagKeyStr
        case HashMap.lookup tagVal parserMap of
          Nothing -> fail . mconcat $
            [ "We are not expecting a payload with tag value " <> backticks tagVal
            , " under the " <> backticks tagKeyStr <> " key here. "
            , "Expected tag values: "
            , intercalate ", " $ backticks <$> HashMap.keys parserMap
            , "."
            ]
          Just parser -> RecordSumEncoded . to <$> parser val
            & prependErrMsg
              ("Failed parsing the case with tag value "
                <> backticks tagVal <> " under the "
                <> backticks tagKeyStr <> " key: ")

        where
          tagKeyStr = symbolVal $ Proxy @tagKey
          ParserMap parserMap
            = unsafeMapKeys (stringFunction $ Proxy @tagModifier)
            . gParserMap
            $ Proxy @(Rep a ())
          backticks str = "`" <> str <> "`"
          prependErrMsg str = modifyFailure (str <>)
          outerErrorMsg = "Failed to parse a " <> datatypeName @meta undefined <> ": "


instance
  ( Generic a
  , GToJSON Zero (Rep a))
    => ToJSON (RecordSumEncoded tagKey tagModifier a) where
      toJSON (RecordSumEncoded x) =
        toJSON $ GenericEncoded @'[SumEncoding := UntaggedValue] x
