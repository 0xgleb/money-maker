module MoneyMaker.ExchangeOverlord.Impl
  ( BinanceT(..)
  -- only export types with the instances we need
  -- then we can derive those instances via these
  -- types without exposing implementation details
  )
  where

-- inner dependencies
import MoneyMaker.ExchangeOverlord.Impl.Internal.Binance

-- internal dependencies
import qualified MoneyMaker.Error            as E
import qualified MoneyMaker.ExchangeOverlord as EO

-- external dependencies
import Protolude

newtype BinanceT m a
  = BinanceT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance EO.MonadExchangeOverlord (BinanceT m) where
  getLatestBTCPrice = undefined

-- getLatestBinanceBTCPrice
--   :: E.MonadUltraError m
--   => m errors EO.BitcoinPrice

-- getLatestBinanceBTCPrice = do
