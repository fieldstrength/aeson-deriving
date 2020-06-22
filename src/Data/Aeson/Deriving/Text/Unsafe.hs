module Data.Aeson.Deriving.Text.Unsafe where

import           Control.Monad   (unless)
import           Data.Aeson
import           Data.Proxy
import           Data.Text       (Text, unpack)
import           Text.Regex.TDFA ((=~))
import           GHC.TypeLits    (KnownSymbol, Symbol, symbolVal)

newtype TextWithPattern (regex :: Symbol) = TextWithPattern Text
  deriving newtype (ToJSON)

instance KnownSymbol regex => FromJSON (TextWithPattern regex) where
  parseJSON = withText "Text" $ \s ->
    TextWithPattern <$> pure s <* unless (unpack s =~ (symbolVal $ Proxy @regex)) (fail errorMsg)
    where
      errorMsg = "must match regex " <> (symbolVal $ Proxy @regex)
