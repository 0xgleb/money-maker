{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.Coinbase.SDK.Rest
  ( CoinbaseRestAPI(..)

  , SandboxCoinbaseRestT(..)

  , Granularity(..)
  )
  where

import MoneyMaker.Coinbase.SDK.Contract

import qualified MoneyMaker.Error as Error

import Protolude

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


type SandboxCoinbaseRestT
  :: ([Type] -> Type -> Type)
  -> [Type]
  -> Type
  -> Type

newtype SandboxCoinbaseRestT m errors a
  = SandboxCoinbaseRestT { runSandboxCoinbaseRestT :: m errors a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, Error.MonadUltraError)

instance
  ( Error.MonadUltraError m
  , forall errors. MonadIO (m errors)
  ) => CoinbaseRestAPI (SandboxCoinbaseRestT m)
  where
    getCandles productId startTime endTime granularity
      = SandboxCoinbaseRestT $ getSandboxCandles productId
          (Just $ UserAgentHeader "Haskell: servant-client")
          (Just startTime)
          (Just endTime)
          (Just granularity)


newtype UserAgentHeader
  = UserAgentHeader Text
  deriving newtype (Servant.ToHttpApiData)

data Granularity
  = OneMinute      -- ^ 60
  | FiveMinutes    -- ^ 300
  | FifteenMinutes -- ^ 900
  | OneHour        -- ^ 3600
  | SixHours       -- ^ 21600
  | OneDay         -- ^ 86400

instance Servant.ToHttpApiData Granularity where
  toUrlPiece = \case
    OneMinute      -> "60"
    FiveMinutes    -> "300"
    FifteenMinutes -> "900"
    OneHour        -> "3600"
    SixHours       -> "21600"
    OneDay         -> "86400"

api :: Proxy API
api = Proxy

type API
  = "products" :> Servant.Capture "product-id" TradingPair :> "candles"
  :> Servant.Header "User-Agent" UserAgentHeader
  :> Servant.QueryParam "start" Time.UTCTime
  :> Servant.QueryParam "end" Time.UTCTime
  :> Servant.QueryParam "granularity" Granularity
  :> Servant.Get '[Servant.JSON] [Candle]

getSandboxCandles
  :: ( Error.MonadUltraError m
     , Servant.ClientError `Error.Elem` errors
     , MonadIO (m errors)
     )
  => TradingPair
  -> Maybe UserAgentHeader
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
