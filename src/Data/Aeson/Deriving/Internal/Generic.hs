{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Deriving.Internal.Generic where

import           Data.Aeson
import           Data.Aeson.Deriving.Known
import           Data.Aeson.Deriving.Internal.RecordSum
import           Data.Aeson.Deriving.Utils
import           Data.Aeson.Types                       (modifyFailure)
import           Data.Char                              (isUpper, toLower, toUpper)
import           Data.Function                          ((&))
import qualified Data.HashMap.Strict                    as HashMap
import           Data.Kind                              (Constraint, Type)
import           Data.List                              (intercalate, stripPrefix)
import           Data.Maybe                             (fromMaybe)
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (pack)
import           GHC.Generics
import           GHC.TypeLits

------------------------------------------------------------------------------------------
-- Main class
------------------------------------------------------------------------------------------

-- | A class for defining 'Options' for Aeson's Generic deriving support.
--   It is generally instantiated by overriding specific fields using the instance
--   for (type-level) list values. It can also be instantiated in a more  exhaustive way
--   using the 'GenericOptions' type. In both cases fields are specified in a record-like
--   form using the '(:=)' data type for explicitness.
--
--   See the ReadMe or tests for examples.
--
--   Users may also provide instances for their own phantom data types if desired.
class ToAesonOptions a where
  toAesonOptions :: Proxy a -> Options

instance ToAesonOptions '[] where toAesonOptions Proxy = defaultOptions
instance (ToAesonOptionsField x, ToAesonOptions xs) => ToAesonOptions (x ': xs) where
  toAesonOptions Proxy =
    let
      patch = toAesonOptionsField (Proxy @x)
      opts = toAesonOptions (Proxy @xs)
    in
      patch $ defaultOptions
        { fieldLabelModifier = fieldLabelModifier opts
        , constructorTagModifier = constructorTagModifier opts
        , allNullaryToStringTag = allNullaryToStringTag opts
        , omitNothingFields = omitNothingFields opts
        , sumEncoding = sumEncoding opts
        , unwrapUnaryRecords = unwrapUnaryRecords opts
        , tagSingleConstructors = tagSingleConstructors opts
        }

-- Its easy to get confusing errors if you forget to tick the list syntax. Hence the custom error
instance TypeError ToAesonOptionsListError => ToAesonOptions [] where toAesonOptions = undefined
instance TypeError ToAesonOptionsListError => ToAesonOptions [a] where toAesonOptions = undefined

type ToAesonOptionsListError =
  (     'Text "aeson-deriving constraint error for ToAesonOptions class:"
  ':$$: 'Text "Don't forget to \"tick\" your opening list bracket."
  ':$$: 'Text "There is no ToAesonOptions instance for list types."
  ':$$: 'Text "Rather, there are instances for promoted list values."
  ':$$: 'Text ""
  ':$$: 'Text "You likely should correct your deriving declaration to something like:"
  ':$$: 'Text ""
  ':$$: 'Text "  via GenericEncoded '[myVal1,..]"
  ':$$: 'Text ""
  ':$$: 'Text "Instead of:"
  ':$$: 'Text ""
  ':$$: 'Text "  via GenericEncoded [myVal1,..]"
  ':$$: 'Text ""
  ':$$: 'Text "For explanation, see GHC documentation on datatype promotion:"
  ':$$: 'Text "https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion"
  ':$$: 'Text ""
  )

-- | A class that knows about fields of aeson's 'Options'.
class ToAesonOptionsField x where
  toAesonOptionsField :: Proxy x -> Options -> Options

-- | Represents an aeson 'Options' field to be set with `(:=)`. See `ToAesonOptions`
data FieldLabelModifier
-- | Represents an aeson 'Options' field to be set with `(:=)`. See `ToAesonOptions`
data ConstructorTagModifier
-- | Represents an aeson 'Options' field to be set with `(:=)`. See `ToAesonOptions`
data AllNullaryToStringTag
-- | Represents an aeson 'Options' field to be set with `(:=)`. See `ToAesonOptions`
data OmitNothingFields

-- SumEncoding type name already exists in aeson. We repurpose it.

-- | Represents an aeson 'Options' field to be set with `(:=)`. See `ToAesonOptions`
data UnwrapUnaryRecords
-- | Represents an aeson 'Options' field to be set with `(:=)`. See `ToAesonOptions`
data TagSingleConstructors

instance StringFunction f => ToAesonOptionsField (FieldLabelModifier := f) where
    toAesonOptionsField Proxy opts = opts {fieldLabelModifier = stringFunction $ Proxy @f}
instance StringFunction f => ToAesonOptionsField (ConstructorTagModifier := f) where
    toAesonOptionsField Proxy opts = opts {constructorTagModifier = stringFunction $ Proxy @f}
instance KnownBool b => ToAesonOptionsField (AllNullaryToStringTag := b) where
    toAesonOptionsField Proxy opts = opts {allNullaryToStringTag = boolVal $ Proxy @b}
instance KnownBool b => ToAesonOptionsField (OmitNothingFields := b) where
    toAesonOptionsField Proxy opts = opts {omitNothingFields = boolVal $ Proxy @b}
instance ToSumEncoding se => ToAesonOptionsField (SumEncoding := se) where
    toAesonOptionsField Proxy opts = opts {sumEncoding = toSumEncoding $ Proxy @se}
instance KnownBool b => ToAesonOptionsField (UnwrapUnaryRecords := b) where
    toAesonOptionsField Proxy opts = opts {unwrapUnaryRecords = boolVal $ Proxy @b}
instance KnownBool b => ToAesonOptionsField (TagSingleConstructors := b) where
    toAesonOptionsField Proxy opts = opts {tagSingleConstructors = boolVal $ Proxy @b}


------------------------------------------------------------------------------------------
-- A Single type for all Options fields
------------------------------------------------------------------------------------------

-- | Type-level representation of the Aeson Generic deriving 'Options'.
--   This representation is useful for explicitly setting all options.
data GenericOptions
  :: fieldLabelModifier
  -> constructorTagModifier
  -> allNullaryToStringTag
  -> omitNothingFields
  -> sumEncoding
  -> unwrapUnaryRecords
  -> tagSingleConstructors
  -> Type

instance
  ( All StringFunction [fieldLabelModifier, constructorTagModifier]
  , ToSumEncoding sumEncoding
  , All KnownBool
     [ allNullaryToStringTag
     , omitNothingFields
     , unwrapUnaryRecords
     , tagSingleConstructors
     ]
  ) => ToAesonOptions
    (GenericOptions
      (FieldLabelModifier := fieldLabelModifier)
      (ConstructorTagModifier := constructorTagModifier)
      (AllNullaryToStringTag := allNullaryToStringTag)
      (OmitNothingFields := omitNothingFields)
      (SumEncoding := sumEncoding)
      (UnwrapUnaryRecords := unwrapUnaryRecords)
      (TagSingleConstructors := tagSingleConstructors)) where
  toAesonOptions _           = defaultOptions
    { fieldLabelModifier     = stringFunction $ Proxy @fieldLabelModifier
    , constructorTagModifier = stringFunction $ Proxy @constructorTagModifier
    , allNullaryToStringTag  = boolVal $ Proxy @allNullaryToStringTag
    , omitNothingFields      = boolVal $ Proxy @omitNothingFields
    , sumEncoding            = toSumEncoding $ Proxy @sumEncoding
    , unwrapUnaryRecords     = boolVal $ Proxy @unwrapUnaryRecords
    , tagSingleConstructors  = boolVal $ Proxy @tagSingleConstructors
    }



-- | Specify your encoding scheme in terms of aeson's out-of-the box Generic
--   functionality. This type is never used directly, only "coerced through".
--   Use some of the pre-defined types supplied here for the @opts@ phantom parameter,
--   or define your with an instance of 'ToAesonOptions'.
newtype GenericEncoded opts a = GenericEncoded a

instance
  ( ToAesonOptions opts
  , Generic a
  , GFromJSON Zero (Rep a))
    => FromJSON (GenericEncoded opts a) where
      parseJSON = fmap GenericEncoded . genericParseJSON (toAesonOptions $ Proxy @opts)

instance
  ( ToAesonOptions opts
  , Generic a
  , GToJSON Zero (Rep a))
    => ToJSON (GenericEncoded opts a) where
      toJSON (GenericEncoded x)
        = genericToJSON (toAesonOptions (Proxy @opts)) x

-- | Used in FromJSON/ToJSON superclass constraints for newtypes that recursively modify
--   the instances. A guard against the common mistake of deriving encoders in terms
--   of such a newtype over the naked base type instead of the 'GenericEncoded' version.
--   This can lead to nasty runtime bugs.
--
--   This error can be disabled by wrapping your type in 'DisableLoopWarning'.
--   This should never be necessary to use the functionality of this package. It may be
--   required if you, for example, combine our newtypes with another library's types
--   for generating aeson instances.
type family LoopWarning (n :: Type -> Type) (a :: Type) :: Constraint where
  LoopWarning n (GenericEncoded opts a) = ()
  LoopWarning n (RecordSumEncoded tagKey tagValMod a) = ()
  LoopWarning n (DisableLoopWarning a) = ()
  LoopWarning n (x & f) = LoopWarning n (f x)
  LoopWarning n (f x) = LoopWarning n x
  LoopWarning n x = TypeError
    ( 'Text "Uh oh! Watch out for those infinite loops!"
    ':$$: 'Text "Newtypes that recursively modify aeson instances, namely:"
    ':$$: 'Text ""
    ':$$: 'Text "  " ':<>: 'ShowType n
    ':$$: 'Text ""
    ':$$: 'Text "must only be used atop a type that creates the instances non-recursively: "
    ':$$: 'Text ""
    ':$$: 'Text "  ￮ GenericEncoded"
    ':$$: 'Text "  ￮ RecordSumEncoded"
    ':$$: 'Text ""
    ':$$: 'Text "We observe instead the inner type: "
    ':$$: 'Text ""
    ':$$: 'Text "  " ':<>: 'ShowType x
    ':$$: 'Text ""
    ':$$: 'Text "You probably created an infinitely recursive encoder/decoder pair."
    ':$$: 'Text "See `LoopWarning` for details."
    ':$$: 'Text "This check can be disabled by wrapping the inner type in `DisableLoopWarning`."
    ':$$: 'Text ""
    )

-- | Assert that you know what you're doing and to nullify the 'LoopWarning' constraint
--   family. This should not be necessary.
newtype DisableLoopWarning a = DisableLoopWarning a
  deriving newtype (FromJSON, ToJSON)

------------------------------------------------------------------------------------------
-- Sums over records
------------------------------------------------------------------------------------------

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
            $ Proxy @(Rep a)
          backticks str = "`" <> str <> "`"
          prependErrMsg str = modifyFailure (str <>)
          outerErrorMsg = "Failed to parse a " <> datatypeName @meta undefined <> ": "


instance
  ( Generic a
  , GToJSON Zero (Rep a))
    => ToJSON (RecordSumEncoded tagKey tagModifier a) where
      toJSON (RecordSumEncoded x) =
        toJSON $ GenericEncoded @'[SumEncoding := UntaggedValue] x



------------------------------------------------------------------------------------------
-- String functions
------------------------------------------------------------------------------------------

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = reverse <$> stripPrefix (reverse a) (reverse b)

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix a b = fromMaybe b $ stripPrefix a b

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a b = fromMaybe b $ stripSuffix a b

class StringFunction (a :: k) where
  stringFunction :: Proxy a -> String -> String

data Id
-- | Applies 'snakeCase'
data SnakeCase
data Uppercase
data Lowercase
-- | Applies 'dropLowercasePrefix', dropping until the first uppercase character.
data DropLowercasePrefix
data DropPrefix (str :: Symbol)
data DropSuffix (str :: Symbol)

instance StringFunction Id where stringFunction _ = id
instance StringFunction SnakeCase where stringFunction _ = snakeCase
instance StringFunction Uppercase where stringFunction _ = map toUpper
instance StringFunction Lowercase where stringFunction _ = map toLower
instance StringFunction DropLowercasePrefix where stringFunction _ = dropLowercasePrefix

instance KnownSymbol str => StringFunction (DropPrefix str) where
  stringFunction Proxy = dropPrefix (symbolVal $ Proxy @str)
instance KnownSymbol str => StringFunction (DropSuffix str) where
  stringFunction Proxy = dropSuffix (symbolVal $ Proxy @str)

instance StringFunction '[] where stringFunction _ = id
instance (StringFunction x, StringFunction xs) => StringFunction (x ': xs) where
    stringFunction Proxy = stringFunction (Proxy @x) . stringFunction (Proxy @xs)

instance All KnownSymbol [a, b] => StringFunction (a ==> b) where
  stringFunction Proxy x
    | x == symbolVal (Proxy @a) = symbolVal (Proxy @b)
    | otherwise = x

------------------------------------------------------------------------------------------
-- Sum type encodings
------------------------------------------------------------------------------------------

-- | Type-level encoding for 'SumEncoding'
class ToSumEncoding a where
  toSumEncoding :: Proxy a -> SumEncoding

data UntaggedValue
data ObjectWithSingleField
data TwoElemArray

-- | A constructor will be encoded to an object with a field tagFieldName which specifies
--   the constructor tag (modified by the constructorTagModifier). If the constructor is
--   a record the encoded record fields will be unpacked into this object. So make sure
--   that your record doesn't have a field with the same label as the tagFieldName.
--   Otherwise the tag gets overwritten by the encoded value of that field! If the
--   constructor is not a record the encoded constructor contents will be stored under
--   the contentsFieldName field.
data TaggedObject (tagFieldName :: Symbol) (contentsFieldName :: Symbol)
-- Would be nice to have separate types for records versus ordinary constructors
-- rather than conflating them with the conditional interpretation of this type.
-- However, this module is just about modeling what aeson gives us.

instance ToSumEncoding UntaggedValue where toSumEncoding _ = UntaggedValue
instance ToSumEncoding ObjectWithSingleField where toSumEncoding _ = ObjectWithSingleField
instance ToSumEncoding TwoElemArray where toSumEncoding _ = TwoElemArray
instance (KnownSymbol tag, KnownSymbol contents) => ToSumEncoding (TaggedObject tag contents) where
  toSumEncoding _ = TaggedObject
    (symbolVal $ Proxy @tag)
    (symbolVal $ Proxy @contents)


------------------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------------------

-- | Field name modifier function that separates camel-case words by underscores
--   (i.e. on capital letters). Also knows to handle a consecutive sequence of
--   capitals as a single word.
snakeCase :: String -> String
snakeCase = camelTo2 '_'

-- | Drop the first lowercase sequence (i.e. until 'isUpper' returns True) from the start
--   of a string. Used for the common idiom where fields are prefixed by the type name in
--   all lowercase. The definition is taken from the aeson-casing package.
dropLowercasePrefix :: String -> String
dropLowercasePrefix [] = []
dropLowercasePrefix (x:xs)
  | isUpper x = x : xs
  | otherwise = dropLowercasePrefix xs

infixl 2 &

newtype (x & f) = Ampersand {unAmpersand :: f x }
  deriving newtype (FromJSON, ToJSON)
