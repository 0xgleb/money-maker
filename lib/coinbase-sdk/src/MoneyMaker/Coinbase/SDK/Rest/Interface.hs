{-# LANGUAGE StrictData #-}

module MoneyMaker.Coinbase.SDK.Rest.Interface
  ( CoinbaseRestAPI(..)

  , Granularity(..)

  , ServantClientError(..)
  , HeaderError(..)
  )
  where

import MoneyMaker.Coinbase.SDK.Contract

import qualified MoneyMaker.Error as Error

import Protolude

import qualified Data.Time.Clock                   as Time
import qualified Servant.API                       as Servant
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC


class Error.MonadUltraError m => CoinbaseRestAPI m where
  getCandles
    :: ( ServantClientError `Error.Elem` errors
       )
    => TradingPair
    -> Maybe (Time.UTCTime)
    -> Maybe (Time.UTCTime)
    -> Granularity
    -> m errors [Candle]

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
