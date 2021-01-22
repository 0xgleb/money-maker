module MoneyMaker.ExchangeOverlord
  ( MonadExchangeOverlord(..)
  , BitcoinPrice(..)
  )
  where

-- internal dependencies
import qualified MoneyMaker.Error as E

-- external dependencies
import Protolude

class E.MonadUltraError m => MonadExchangeOverlord m where
  getLatestBTCPrice
    :: m errors BitcoinPrice

newtype BitcoinPrice
  = BitcoinPrice { getBitcoinPrice :: Float }
