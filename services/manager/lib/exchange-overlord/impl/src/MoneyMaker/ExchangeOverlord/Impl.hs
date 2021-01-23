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

newtype BinanceT (m :: [Type] -> Type -> Type) (errors :: [Type]) (a :: Type)
  = BinanceT (m errors a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance E.MonadUltraError m => E.MonadUltraError (BinanceT m)

instance E.MonadUltraError m => EO.MonadExchangeOverlord (BinanceT m) where
  getLatestBTCPrice = BinanceT $ getLatestBinanceBTCPrice

getLatestBinanceBTCPrice
  :: ( E.MonadUltraError m
     , MonadIO (m errors)
     )
  => m errors EO.BitcoinPrice

getLatestBinanceBTCPrice = do
  TickerPrice{price = Price price} <- getBinanceTickerSymbol
  pure $ EO.BitcoinPrice price
