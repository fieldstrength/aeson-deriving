{-# Language DerivingVia #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveAnyClass #-}
{-# Language DuplicateRecordFields #-}

module Main where

import Data.Aeson
import Data.Aeson.Deriving
import GHC.Generics
import Hedgehog
import Hedgehog.Main (defaultMain)

main :: IO ()
main = defaultMain [checkParallel $$(discover)]

type IdiomaticEncoded =
  GenericEncoded '[FieldLabelModifier := '[SnakeCase, DropLowercasePrefix]]

data Dog = Dog
  { dogAgeInDogYears :: Int
  , dogName :: String
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via IdiomaticEncoded Dog

once :: Property -> Property
once = withTests 1

prop_fido_encodes_as_expected :: Property
prop_fido_encodes_as_expected = once . property $
  encode (Dog 9 "fido") === "{\"name\":\"fido\",\"age_in_dog_years\":9}"

prop_fido_decodes_as_expected :: Property
prop_fido_decodes_as_expected = once . property $
  tripping (Dog 9 "fido") encode eitherDecode


type UppercaseTypeTagEncoded =
  GenericEncoded
    '[ FieldLabelModifier := '[SnakeCase, DropLowercasePrefix]
    ,  SumEncoding := TaggedObject "type" "contents"
    ,  TagSingleConstructors := 'True
    ,  ConstructorTagModifier := '[Uppercase, SnakeCase]
    ]

data PostArticle = PostArticle
  { articleName :: String
  , articleText :: String
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via UppercaseTypeTagEncoded PostArticle

data DeleteArticle = DeleteArticle
  { articleId :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via UppercaseTypeTagEncoded DeleteArticle

data ArticleCommand
  = MkPostArticle PostArticle
  | MkDeleteArticle DeleteArticle
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via RecordSumEncoded "type" '[Uppercase, SnakeCase] ArticleCommand

prop_record_sum_encodes_as_expected :: Property
prop_record_sum_encodes_as_expected = once . property $
  encode (MkDeleteArticle $ DeleteArticle 9)
    === "{\"id\":9,\"type\":\"DELETE_ARTICLE\"}"

prop_record_sum_decodes_as_expected :: Property
prop_record_sum_decodes_as_expected = once . property $
  tripping (MkDeleteArticle $ DeleteArticle 9) encode decode


data MyVal
instance KnownJSON MyVal where jsonVal _ = Number 1

data X = X {xval :: Int}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    WithConstantFields
      '["bar" := "baaz", "quux" := MyVal, "arr" := ["Hilbert","Dirac"]]
      (GenericEncoded '[] X)

prop_WithConstantFields_extra_fields_encode_as_expected :: Property
prop_WithConstantFields_extra_fields_encode_as_expected = once . property $
  encode (X 9)
    === "{\"xval\":9,\"arr\":[\"Hilbert\",\"Dirac\"],\"quux\":1,\"bar\":\"baaz\"}"

prop_WithConstantFields_extra_fields_decode_as_expected :: Property
prop_WithConstantFields_extra_fields_decode_as_expected = once . property $
  tripping (X 9) encode decode

prop_WithConstantFields_extra_fields_required_when_decoding :: Property
prop_WithConstantFields_extra_fields_required_when_decoding = once . property $
  decode @X "{\"xval\":9}" === Nothing

data X2 = X2 {xval :: Int}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    WithConstantFieldsOut
      '["bar" := "baaz", "quux" := "axion"]
      (GenericEncoded '[] X2)

prop_WithConstantFieldsOut_encodes_as_expected :: Property
prop_WithConstantFieldsOut_encodes_as_expected = once . property $
  encode (X2 9)
    === "{\"xval\":9,\"quux\":\"axion\",\"bar\":\"baaz\"}"

prop_WithConstantFieldsOut_extra_fields_not_required_when_decoding :: Property
prop_WithConstantFieldsOut_extra_fields_not_required_when_decoding = once . property $
  decode @X2 "{\"xval\":9}" === Just (X2 9)

data X3 = X3 {xval :: Int}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    WithConstantFieldsIn
      '["bar" := "baaz", "quux" := "axion"]
      (GenericEncoded '[] X3)

prop_WithConstantFieldsIn_encodes_as_expected :: Property
prop_WithConstantFieldsIn_encodes_as_expected = once . property $
  encode (X3 13)
    === "{\"xval\":13}"

prop_WithConstantFieldsIn_decodes_as_expected :: Property
prop_WithConstantFieldsIn_decodes_as_expected = once . property $
  decode @X3 "{\"xval\":9,\"quux\":\"axion\",\"bar\":\"baaz\"}" === Just (X3 9)

prop_WithConstantFieldsIn_extra_fields_required_when_decoding :: Property
prop_WithConstantFieldsIn_extra_fields_required_when_decoding = once . property $
  decode @X3 "{\"xval\":9}" === Nothing

data Y = Y {yval :: Int}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    SingleFieldObject "boop" (GenericEncoded '[] Y)

prop_single_field_objects_encode_as_expected :: Property
prop_single_field_objects_encode_as_expected = once . property $
  encode (Y 7)
    === "{\"boop\":{\"yval\":7}}"

prop_single_field_objects_decode_as_expected :: Property
prop_single_field_objects_decode_as_expected = once . property $
  tripping (Y 7) encode decode
