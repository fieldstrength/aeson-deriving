{-# Language DerivingVia #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveAnyClass #-}
{-# Language DuplicateRecordFields #-}

module Main where

import Data.Aeson
import Data.Aeson.Deriving
import Data.Foldable (for_)
import Data.Aeson.Deriving.Text.Unsafe
import Data.Text
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
  encode (Dog 9 "fido") === "{\"age_in_dog_years\":9,\"name\":\"fido\"}"

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
    X
      & GenericEncoded '[]
      & WithConstantFields
          '["bar" := "baaz", "quux" := MyVal, "arr" := ["Hilbert","Dirac"]]

prop_WithConstantFields_extra_fields_encode_as_expected :: Property
prop_WithConstantFields_extra_fields_encode_as_expected = once . property $
  encode (X 9)
    === "{\"arr\":[\"Hilbert\",\"Dirac\"],\"bar\":\"baaz\",\"quux\":1,\"xval\":9}"

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
    === "{\"bar\":\"baaz\",\"quux\":\"axion\",\"xval\":9}"

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
  decode @X3 "{\"bar\":\"baaz\",\"quux\":\"axion\",\"xval\":9}" === Just (X3 9)

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

data Z = Z {zval :: String}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    RemapTextField "zval" "bad" "good" (GenericEncoded '[] Z)

prop_remapped_text_fields_encode_as_expected :: Property
prop_remapped_text_fields_encode_as_expected = once . property $ do
  encode (Z "bad") === "{\"zval\":\"good\"}"
  encode (Z "cat") === "{\"zval\":\"cat\"}"

prop_remapped_text_fields_decode_as_expected :: Property
prop_remapped_text_fields_decode_as_expected = once . property $ do
  tripping (Z "bad") encode decode
  tripping (Z "cat") encode decode
  Just (Z "bad") === decode "{\"zval\":\"good\"}"
  Just (Z "cat") === decode "{\"zval\":\"cat\"}"

data Reserved = Reserved
  { type_ :: String
  , xyzmodule :: Int
  , control :: Char
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via
    Reserved &
      GenericEncoded
        '[FieldLabelModifier := [DropPrefix "xyz", DropSuffix "_"]]

prop_drop_prefix_suffix_fields_encode_as_expected :: Property
prop_drop_prefix_suffix_fields_encode_as_expected = once . property $ do
  encode (Reserved "Sen" 9 'x') ===
    "{\"control\":\"x\",\"module\":9,\"type\":\"Sen\"}"

prop_drop_prefix_suffix_fields_decode_as_expected :: Property
prop_drop_prefix_suffix_fields_decode_as_expected = once . property $ do
  tripping (Reserved "Sen" 9 'x') encode decode
  Just (Reserved "Sen" 9 'x') ===
    decode "{\"control\":\"x\",\"module\":9,\"type\":\"Sen\"}"

newtype DashSeparatedWords = DashSeparatedWords Text
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via TextWithPattern "^([A-Za-z]+-)*[A-Za-z]+$"

prop_accepts_matches :: Property
prop_accepts_matches = once . property $ do
  tripping (DashSeparatedWords "foo-bar-baz") encode decode
  Just (DashSeparatedWords "foo-bar-baz") === decode "\"foo-bar-baz\""

prop_rejects_non_matches :: Property
prop_rejects_non_matches = once . property $ do
  Left "Error in $: must match regex ^([A-Za-z]+-)*[A-Za-z]+$" === eitherDecode @DashSeparatedWords "\"foo.42\""


data Heavy = BlackHole | NeutronStar
 deriving stock (Generic, Show, Eq, Ord, Bounded, Enum)
 deriving (FromJSON, ToJSON) via GenericEncoded '[ConstructorTagModifier := FirstChar Lowercase] Heavy

prop_first_char_modifier_encodes_as_expected :: Property
prop_first_char_modifier_encodes_as_expected = once . property $ do
  encode BlackHole === "\"blackHole\""
  encode NeutronStar === "\"neutronStar\""

prop_first_char_modifier_decodes_as_expected :: Property
prop_first_char_modifier_decodes_as_expected = once . property $ do
  for_ [BlackHole ..] $ \x ->
    tripping x encode decode


data Unity = Unity
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via EmptyObject Unity

prop_empty_object_encodes_as_expected :: Property
prop_empty_object_encodes_as_expected = once . property $ do
  encode Unity === "{}"

prop_empty_object_decodes_as_expected :: Property
prop_empty_object_decodes_as_expected = once . property $ tripping Unity encode decode

-- An example of how to require a particular constant object
data Requirements = Requirements
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via
    WithConstantFields
     '[ "api_version" := "2.0"
      , "check_performed" := 'True
      ]
      (EmptyObject Requirements)

prop_constant_object_encodes_as_expected :: Property
prop_constant_object_encodes_as_expected = once . property $
  encode Requirements === "{\"api_version\":\"2.0\",\"check_performed\":true}"

prop_constant_object_decodes_as_expected :: Property
prop_constant_object_decodes_as_expected = once . property $ tripping Requirements encode decode

prop_reject_constant_object_with_incorrect_details :: Property
prop_reject_constant_object_with_incorrect_details = once . property $
  eitherDecode @Requirements "{\"api_version\":\"2.0\",\"check_performed\":false}"
    === Left "Error in $: Expected constant value \"true\" but got: \"false\""
