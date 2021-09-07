{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.Coinbase.SDK.Rest
  ( CoinbaseRestAPI(..)

  , SandboxCoinbaseRestT(..)

  , Granularity(..)

  , Candle(..)
  , consolidateCandles
  )
  where

import MoneyMaker.Coinbase.SDK.Model

import qualified MoneyMaker.Error as Error

import Data.Vector ((!?))
import Protolude

import qualified Control.Monad.Fail      as Fail
import qualified Data.Aeson              as Aeson
import qualified Data.Time.Clock         as Time
import qualified Network.HTTP.Client     as Network
import qualified Network.HTTP.Client.TLS as Network.TLS
import           Servant.API             ((:>))
import qualified Servant.API             as Servant
import qualified Servant.Client          as Servant
import qualified Timestamp

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

api :: Proxy API
api = Proxy

type API
  = "products" :> Servant.Capture "product-id" TradingPair :> "candles"
  :> Servant.Header "User-Agent" UserAgentHeader
  :> Servant.QueryParam "start" Time.UTCTime
  :> Servant.QueryParam "end" Time.UTCTime
  :> Servant.QueryParam "granularity" Granularity
  :> Servant.Get '[Servant.JSON] [Candle]

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

data Candle
  = Candle
      { time  :: Time.UTCTime
      , low   :: Price
      , high  :: Price
      , open  :: Price
      , close :: Price
      -- , volume :: Volume
      }
  deriving stock (Generic, Show, Eq)

instance Aeson.FromJSON Candle where
  parseJSON = Aeson.withArray "Candle" $ \array ->
    case (array !? 0, array !? 1, array !? 2, array !? 3, array !? 4) of
      (Just jsonTime, Just jsonLow, Just jsonHigh, Just jsonOpen, Just jsonClose) -> do
        time <-
          Timestamp.timestampUtcTime . Timestamp.Timestamp
            <$> Aeson.parseJSON jsonTime

        low   <- Price <$> Aeson.parseJSON jsonLow
        high  <- Price <$> Aeson.parseJSON jsonHigh
        open  <- Price <$> Aeson.parseJSON jsonOpen
        close <- Price <$> Aeson.parseJSON jsonClose

        pure Candle{..}

      _ -> Fail.fail $ "Invalid candle array: " <> show array


consolidateCandles :: [Candle] -> Maybe Candle
consolidateCandles
  = flip foldl Nothing $ \consolidatedCandle nextCandle ->
      case consolidatedCandle of
        Nothing -> Just nextCandle
        Just consolidated@Candle{time} ->
          Just Candle
            { low =
                if low nextCandle < low consolidated
                then low nextCandle
                else low consolidated
            , high =
                if high nextCandle > high consolidated
                then high nextCandle
                else high consolidated
            , open  = open consolidated
            , close = close nextCandle
            , time
            }
