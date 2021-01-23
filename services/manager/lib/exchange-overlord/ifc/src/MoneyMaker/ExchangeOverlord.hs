module MoneyMaker.ExchangeOverlord
  ( MonadExchangeOverlord(..)
  , BitcoinPrice(..)
  , GetLatestBTCPriceError(..)
  )
  where

-- internal dependencies
import qualified MoneyMaker.Error as E

-- external dependencies
import Protolude
import qualified Data.Aeson as Aeson

class E.MonadUltraError m => MonadExchangeOverlord m where
  getLatestBTCPrice
    :: GetLatestBTCPriceError `E.Elem` errors
    => m errors BitcoinPrice

newtype BitcoinPrice
  = BitcoinPrice { getBitcoinPrice :: Float }

data GetLatestBTCPriceError
  = GetLatestBTCPriceError Aeson.Value
