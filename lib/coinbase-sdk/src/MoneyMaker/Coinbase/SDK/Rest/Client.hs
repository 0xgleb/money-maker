{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.Coinbase.SDK.Rest.Client
  ( SandboxCoinbaseRestT(..)
  , ProductionCoinbaseRestT(..)

  , PaginationId(..)
  )
  where

import MoneyMaker.Coinbase.SDK.Contract
import MoneyMaker.Coinbase.SDK.Rest.Interface

import MoneyMaker.Based

import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Time.Clock         as Time
import qualified Network.HTTP.Client     as Network
import qualified Network.HTTP.Client.TLS as Network.TLS
import           Servant.API             ((:>))
import qualified Servant.API             as Servant
import qualified Servant.Client          as Servant


class HasBaseUrlHost (m :: [Type] -> Type -> Type) where
  getBaseUrlHost :: m errors [Char]

type SandboxCoinbaseRestT
  :: ([Type] -> Type -> Type)
  -> [Type]
  -> Type
  -> Type

newtype SandboxCoinbaseRestT m errors a
  = SandboxCoinbaseRestT { runSandboxCoinbaseRestT :: m errors a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUltraError)

instance (forall errors. Monad (m errors))
  => HasBaseUrlHost (SandboxCoinbaseRestT m)
  where
    getBaseUrlHost
      = pure "api-public.sandbox.pro.coinbase.com"

type ProductionCoinbaseRestT
  :: ([Type] -> Type -> Type)
  -> [Type]
  -> Type
  -> Type

newtype ProductionCoinbaseRestT m errors a
  = ProductionCoinbaseRestT { runProductionCoinbaseRestT :: m errors a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUltraError)

instance (forall errors. Monad (m errors))
  => HasBaseUrlHost (ProductionCoinbaseRestT m)
  where
    getBaseUrlHost
      = pure "api.pro.coinbase.com"


instance {-# OVERLAPPABLE #-}
  ( MonadUltraError m
  , forall errors. MonadIO (m errors)
  , HasBaseUrlHost m
  ) => CoinbaseRestAPI m
  where
    getCandles productId startTime endTime granularity
      = getCandlesServant productId
          (Just $ UserAgentHeader "Haskell: servant-client")
          (Just startTime)
          (Just endTime)
          (Just granularity)


newtype UserAgentHeader
  = UserAgentHeader Text
  deriving newtype (Servant.ToHttpApiData)

api :: Proxy API
api = Proxy

newtype PaginationId
  = PaginationId { getPaginationId :: Integer }
  deriving newtype (Show, Servant.ToHttpApiData, Servant.FromHttpApiData)

type API
  = "products" :> Servant.Capture "product-id" TradingPair :> "candles"
  :> Servant.Header "User-Agent" UserAgentHeader
  :> Servant.QueryParam "start" Time.UTCTime
  :> Servant.QueryParam "end" Time.UTCTime
  :> Servant.QueryParam "granularity" Granularity
  :> Servant.Get '[Servant.JSON] [Candle]

       -- (Servant.Headers '[Servant.Header "CB-AFTER" PaginationId] )

getCandlesServant
  :: ( MonadUltraError m
     , ServantClientError `Elem` errors
     , HasBaseUrlHost m
     , MonadIO (m errors)
     )
  => TradingPair
  -> Maybe UserAgentHeader
  -> Maybe Time.UTCTime
  -> Maybe Time.UTCTime
  -> Maybe Granularity
  -> m errors [Candle]

  -- (Servant.Headers '[Servant.Header "CB-AFTER" PaginationId] [Candle])

getCandlesServant
  = Servant.hoistClient api naturalTransformation (Servant.client api)
  where
    naturalTransformation
      :: ( MonadUltraError m
         , ServantClientError `Elem` errors
         , HasBaseUrlHost m
         , MonadIO (m errors)
         )
      => Servant.ClientM a
      -> m errors a

    naturalTransformation client = do
      manager <- liftIO $ Network.newManager Network.TLS.tlsManagerSettings

      baseUrlHost <- getBaseUrlHost

      let baseUrl = Servant.BaseUrl
            { baseUrlScheme = Servant.Https
            , baseUrlHost
            , baseUrlPort   = 443
            , baseUrlPath   = ""
            }

      let clientEnv = Servant.mkClientEnv manager baseUrl

      result <- liftIO $ Servant.runClientM client clientEnv

      case result of
        Right success ->
          pure success

        Left clientError ->
          throwUltraError $ case clientError of
            Servant.FailureResponse _request response ->
              FailureResponse $ BSL.toStrict $ Servant.responseBody response

            Servant.DecodeFailure _text response ->
              DecodeFailure $ BSL.toStrict $ Servant.responseBody response

            Servant.UnsupportedContentType _mediaType response ->
              UnsupportedContentType $ BSL.toStrict
                $ Servant.responseBody response

            Servant.InvalidContentTypeHeader response ->
              InvalidContentTypeHeader $ BSL.toStrict
                $ Servant.responseBody response

            Servant.ConnectionError _ ->
              ConnectionError
