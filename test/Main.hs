{-# Language DerivingVia #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveAnyClass #-}

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
instance ToConstant MyVal where toConstant _ = Number 1

data X = X {xval :: Int}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    WithConstantFields
      '["bar" := "baaz", "quux" := MyVal]
      (GenericEncoded '[] X)

prop_constant_fields_encode_as_expected :: Property
prop_constant_fields_encode_as_expected = once . property $
  encode (X 9)
    === "{\"xval\":9,\"quux\":1,\"bar\":\"baaz\"}"

prop_constant_fields_decode_as_expected :: Property
prop_constant_fields_decode_as_expected = once . property $
  tripping (X 9) encode decode


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
