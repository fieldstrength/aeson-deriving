{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

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


newtype RecordSumEncoded (tagKey :: Symbol) (a :: Type) = RecordSumEncoded a

instance
  ( Generic a
  , GFromJSON Zero (Rep a)
  , GTagParserMap (Rep a)
  , Rep a ~ D1 meta cs
  , Datatype meta
  , KnownSymbol tagKey)
    => FromJSON (RecordSumEncoded tagKey a) where
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
          ParserMap parserMap = gParserMap $ Proxy @(Rep a ())
          backticks str = "`" <> str <> "`"
          prependErrMsg str = modifyFailure (str <>)
          outerErrorMsg = "Failed to parse a " <> datatypeName @meta undefined <> ": "


instance
  ( Generic a
  , GToJSON Zero (Rep a))
    => ToJSON (RecordSumEncoded tagKey a) where
      toJSON (RecordSumEncoded x) = toJSON (GenericEncoded @'[SumEncoding := UntaggedValue] x)
