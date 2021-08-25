{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.Coinbase.SDK.Rest
  ( CoinbaseRestAPI(..)

  , SandboxCoinbaseRestT(..)

  , Candle(..)
  , Granularity(..)
  )
  where

import MoneyMaker.Coinbase.SDK.Model

import qualified MoneyMaker.Error as Error

import Protolude

import qualified Data.Aeson              as Aeson
import qualified Data.Time.Clock         as Time
import qualified Network.HTTP.Client     as Network
import qualified Network.HTTP.Client.TLS as Network.TLS
import           Servant.API             ((:>))
import qualified Servant.API             as Servant
import qualified Servant.Client          as Servant

class Error.MonadUltraError m => CoinbaseRestAPI m where
  getCandles
    :: Servant.ClientError `Error.Elem` errors
    => TradingPair
    -> Time.UTCTime
    -> Time.UTCTime
    -> Granularity
    -> m errors [Candle]

newtype SandboxCoinbaseRestT (m :: [Type] -> Type -> Type) (errors :: [Type]) (a :: Type)
  = SandboxCoinbaseRestT { runSandboxCoinbaseRestT :: m errors a }
  deriving newtype (Functor, Applicative, Monad, Error.MonadUltraError)

instance
  ( Error.MonadUltraError m
  , forall errors. MonadIO (m errors)
  ) => CoinbaseRestAPI (SandboxCoinbaseRestT m)
  where
    getCandles productId startTime endTime granularity
      = SandboxCoinbaseRestT $ getSandboxCandles productId
          (Just startTime)
          (Just endTime)
          (Just granularity)

getSandboxCandles
  :: ( Error.MonadUltraError m
     , Servant.ClientError `Error.Elem` errors
     , MonadIO (m errors)
     )
  => TradingPair
  -> Maybe Time.UTCTime
  -> Maybe Time.UTCTime
  -> Maybe Granularity
  -> m errors [Candle]

getSandboxCandles
  = Servant.hoistClient api naturalTransformation (Servant.client api)
  where
    naturalTransformation
      :: ( Error.MonadUltraError m
         , Servant.ClientError `Error.Elem` errors
         , MonadIO (m errors)
         )
      => Servant.ClientM a
      -> m errors a

    naturalTransformation client = do
      manager <- liftIO $ Network.newManager Network.TLS.tlsManagerSettings

      let baseUrl = Servant.BaseUrl
            { baseUrlScheme = Servant.Https
            , baseUrlHost   = "api-public.sandbox.pro.coinbase.com"
            , baseUrlPort   = 443
            , baseUrlPath   = ""
            }

      let clientEnv = Servant.mkClientEnv manager baseUrl

      result <- liftIO $ Servant.runClientM client clientEnv

      case result of
        Left clientError ->
          Error.throwUltraError clientError
        Right success ->
          pure success

api :: Proxy API
api = Proxy

type API
  = "products" :> Servant.Capture "product-id" TradingPair :> "candles"
  :> Servant.QueryParam "start" Time.UTCTime
  :> Servant.QueryParam "end" Time.UTCTime
  :> Servant.QueryParam "granularity" Granularity
  :> Servant.Get '[Servant.JSON] [Candle]

-- TODO: add a custom JSON instance
data Candle
  = Candle
      { time  :: Time.UTCTime
      , low   :: Price
      , high  :: Price
      , open  :: Price
      , close :: Price
      -- , volume :: Volume
      }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON)

data Granularity
  = OneMinute -- ^ 60
  | FiveMinutes -- ^ 300
  | FifteenMinutes -- ^ 900
  | OneHour -- ^ 3600
  | SixHours -- ^ 21600
  | OneDay -- ^ 86400

instance Servant.ToHttpApiData Granularity where
  toUrlPiece = \case
    OneMinute      -> "60"
    FiveMinutes    -> "300"
    FifteenMinutes -> "900"
    OneHour        -> "3600"
    SixHours       -> "21600"
    OneDay         -> "86400"
