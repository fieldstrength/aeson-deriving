# aeson-deriving

[![Build Status](https://travis-ci.org/fieldstrength/aeson-deriving.svg?branch=master)](https://travis-ci.org/fieldstrength/aeson-deriving)
[![Hackage](https://img.shields.io/hackage/v/aeson-deriving.svg)](http://hackage.haskell.org/package/aeson-deriving)

Define JSON encoding and decoding behavior in a unified way with [DerivingVia](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-via). This ensures the instances for the two `aeson` type classes stay in sync and eliminates much needless boilerplate, besides supporting many extra features.

## Uses and examples

### Basic encoding options & common patterns

Aeson's generics support governs the basic mapping between Haskell definitions and the JSON format.
This functionality, along with its tunable parameters, can be specified with the `GenericEncoded` newtype.

```haskell
type MyEncoding = GenericEncoded
  '[ ConstructorTagModifier := SnakeCase  -- extensible function support
  , FieldLabelModifier :=
      [ SnakeCase, DropSuffix "_" ]       -- functions can be composed
  , SumEncoding := TaggedObject "type" "contents"
  ]


data User = User
  { firstName :: Text
  , id_       :: UserId
  , companyId :: CompanyId
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON)
    via MyEncoding User

data Document = Document
  { name      :: Text
  , id_       :: Int64
  , companyId :: CompanyId
  , parts     :: [SubDocument]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON)
    via MyEncoding Document

-- >>> encode (User "jake" 1 29)
-- { "type": "user", "first_name": "jake", "id": 1, "company_id": 29}
```

### Modifier newtypes

#### Constrant Fields

```haskell
data Transaction = Transaction
  { transactionId :: UUID }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via
    WithConstantFieldsOut
     '[ "version" := "1.0"
      , "system_info" := "👍"
      ]
      (MyEncoding Transaction)
```

Note: Some newtypes that modify the instances come in an inbound and outbound variant. For example `WithConstantFields` is defined as the composition of `WithConstantFieldsIn` and `WithConstantFieldsOut`.

#### Constant Objects

Sometimes you may need an entire object of constant fields, with no information passing to the haskell representation. This is modeled as a single-value type and can also be used with the `WithConstantFields` newtype, as long as the base type is wrapped in the `EmptyObject` newtype (because otherwise unit types do not serialize to the empty object by default).

```haskell
data Requirements = Requirements
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via
    WithConstantFields
     '[ "api_version" := "2.0"
      , "check_performed" := 'True
      ]
      (EmptyObject Requirements)
```

#### Apply arbitrary functions before encoding/decoding

##### Example: Special treatment for magic values

```haskell
data Feedback = Feedback
  { comment :: Text }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via
    ModifyFieldIn "comment"
      ("booo!" ==> "boo-urns!")
      (MyEncoding Feedback)


-- x ==> y  maps the value x to y and leaves others unchanged
-- Implement your own instances of `KnownJSONFunction` for other behavior

```

### Preventing infinite loops

Newtypes that modify an inner type class instance must be careful not to do so in an infinitely recursive way. Here the inner type should use the generic-based instance, rather than reference the instance being defined.

This package employs a custom compiler error to prevent this very easy mistake.

### Improved error messages for sums of records

See `RecordSumEncoded` documentation.

To be expanded...
