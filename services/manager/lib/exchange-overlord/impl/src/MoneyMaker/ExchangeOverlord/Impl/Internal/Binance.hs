{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}

module MoneyMaker.ExchangeOverlord.Impl.Internal.Binance
  ( getBinanceTickerSymbol

  , TickerPrice(..)
  , TickerSymbol(..)
  , Price(..)
  )
  where

import qualified MoneyMaker.Error as E

import qualified Data.Aeson          as Aeson
import qualified Network.HTTP.Client as HTTP.Client
import           Protolude
import           Servant.API
import qualified Servant.Client      as Client

customClient
  :: forall api errors m
   . ( Client.HasClient Client.ClientM api
     , E.MonadUltraError m
     , MonadIO (m errors)
     , Client.ClientError `E.Elem` errors
     )
  => Client.Client (m errors) api
customClient
  = Client.hoistClient api naturalTransformation $ Client.client api
  where
    api = Proxy @api
    naturalTransformation :: Client.ClientM a -> m errors a
    naturalTransformation clientAction = do
      manager <- liftIO $ HTTP.Client.newManager HTTP.Client.defaultManagerSettings
      let clientEnv = Client.mkClientEnv manager baseUrl
          baseUrl = Client.BaseUrl
            { baseUrlScheme = Client.Https
            , baseUrlHost   = "api3.binance.com"
            , baseUrlPort   = 80 -- not sure about this
            , baseUrlPath   = "api/v3"
            }
      result <- liftIO $ Client.runClientM clientAction clientEnv
      case result of
        Left clientError ->
          E.throwUltraError clientError
        Right response ->
          pure response

getBinanceTickerSymbol
  :: ( E.MonadUltraError m
     , MonadIO (m errors)
     , Client.ClientError `E.Elem` errors
     )
  => m errors TickerPrice
getBinanceTickerSymbol
  = customClient @BinanceAPI

type BinanceAPI
  = "ticker" :> TickerAPI

type TickerAPI
  = "price" :> Get '[JSON] TickerPrice

data TickerPrice
  = TickerPrice
      { symbol :: TickerSymbol
      , price  :: Price
      }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

newtype TickerSymbol
  = TickerSymbol { getTickerSymbol :: Text }
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype Price
  = Price { getPrice :: Float }
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
