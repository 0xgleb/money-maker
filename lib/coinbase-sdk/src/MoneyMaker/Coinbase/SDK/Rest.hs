{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.Coinbase.SDK.Rest
  ( CoinbaseRestAPI(..)

  , ServantClientError(..)
  , HeaderError(..)

  , SandboxCoinbaseRestT(..)

  , PaginationId(..)
  , Granularity(..)
  )
  where

import MoneyMaker.Coinbase.SDK.Contract

import qualified MoneyMaker.Error as Error

import Protolude

import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Time.Clock                   as Time
import qualified Network.HTTP.Client               as Network
import qualified Network.HTTP.Client.TLS           as Network.TLS
import           Servant.API                       ((:>))
import qualified Servant.API                       as Servant
import qualified Servant.Client                    as Servant
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC


data ServantClientError
  = FailureResponse ByteString
  | DecodeFailure ByteString
  | UnsupportedContentType ByteString
  | InvalidContentTypeHeader ByteString
  | ConnectionError
  deriving stock (Show)

data HeaderError
  = MissingHeaderError
  | UndecodableHeader ByteString
  deriving stock (Show)

class Error.MonadUltraError m => CoinbaseRestAPI m where
  getCandles
    :: ( ServantClientError `Error.Elem` errors
       )
    => TradingPair
    -> Maybe (Time.UTCTime)
    -> Maybe (Time.UTCTime)
    -> Granularity
    -> m errors [Candle]


type SandboxCoinbaseRestT
  :: ([Type] -> Type -> Type)
  -> [Type]
  -> Type
  -> Type

-- TODO: add the production version of the API
newtype SandboxCoinbaseRestT m errors a
  = SandboxCoinbaseRestT { runSandboxCoinbaseRestT :: m errors a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, Error.MonadUltraError)

instance
  ( Error.MonadUltraError m
  , forall errors. MonadIO (m errors)
  ) => CoinbaseRestAPI (SandboxCoinbaseRestT m)
  where
    getCandles productId startTime endTime granularity
      = SandboxCoinbaseRestT
      $ Error.catchUltraError @Servant.ClientError getCandlesAction
      $ \servantError -> Error.throwUltraError $ case servantError of
          Servant.FailureResponse _request response ->
            FailureResponse $ BSL.toStrict $ Servant.responseBody response

          Servant.DecodeFailure _text response ->
            DecodeFailure $ BSL.toStrict $ Servant.responseBody response

          Servant.UnsupportedContentType _mediaType response ->
            UnsupportedContentType $ BSL.toStrict $ Servant.responseBody response

          Servant.InvalidContentTypeHeader response ->
            InvalidContentTypeHeader $ BSL.toStrict $ Servant.responseBody response

          Servant.ConnectionError _ ->
            ConnectionError

      where
        getCandlesAction
          = getSandboxCandles productId
              (Just $ UserAgentHeader "Haskell: servant-client")
              startTime
              endTime
              -- (Just startTime)
              -- (Just endTime)
              (Just granularity)

        -- getCandlesAction = do
        --   Servant.Headers{ ..} <-
        --     getSandboxCandles productId
        --       (Just $ UserAgentHeader "Haskell: servant-client")
        --       afterPaginationId
        --       startTime
        --       endTime
        --       -- (Just startTime)
        --       -- (Just endTime)
        --       (Just granularity)

        --   let responseHeader = case getHeadersHList of
        --         Servant.HCons responseHeader _ ->
        --           responseHeader

        --   paginationId <- case responseHeader of
        --     Servant.Header paginationId ->
        --       pure paginationId
        --     Servant.MissingHeader ->
        --       Error.throwUltraError MissingHeaderError
        --     Servant.UndecodableHeader bytestring ->
        --       Error.throwUltraError $ UndecodableHeader bytestring

        --   pure (getResponse, paginationId)


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
  deriving stock (Generic, Show, Eq)

instance QC.Arbitrary Granularity where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

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

  -- (Servant.Headers '[Servant.Header "CB-AFTER" PaginationId] [Candle])

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
            -- , baseUrlHost   = "api.pro.coinbase.com"
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
