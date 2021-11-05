{-# LANGUAGE StrictData #-}

module MoneyMaker.Coinbase.SDK.Contract
  ( Price(..)
  , Currency(..)
  , TradingPair(..)
  , Candle(..)
  -- , consolidateCandles
  )
  where

import Data.Vector ((!?))
import Protolude

import qualified Control.Monad.Fail                as Fail
import qualified Data.Aeson                        as Aeson
import qualified Data.Fixed                        as Fixed
import qualified Data.Text                         as Txt
import qualified Data.Time.Clock                   as Time
import qualified Data.Time.Clock.POSIX             as Time
import qualified GHC.Show                          as Show
import qualified Servant.API                       as Servant
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC
import           Test.QuickCheck.Instances         ()

-- TODO: this price type is only good when USD is the quote currency
-- pretty much any other quote currency will have more than 2 decimal places
newtype Price
  = Price { getPrice :: Fixed.Centi }
  deriving newtype
    ( QC.Arbitrary
    , Show
    , Read
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    )

data Currency
  = BTC
  | ETH
  | USD
  | EUR
  deriving stock (Generic, Eq, Show, Read)

instance QC.Arbitrary Currency where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

data TradingPair
  = TradingPair
      { baseCurrency  :: Currency -- ^ BTC in BTC/USD
      , quoteCurrency :: Currency -- ^ USD in BTC/USD
      }
  deriving stock (Eq, Generic)

instance Show.Show TradingPair where
  show TradingPair{..}
    = show baseCurrency <> "/" <> show quoteCurrency

instance QC.Arbitrary TradingPair where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

instance Aeson.ToJSON TradingPair where
  toJSON TradingPair{..}
    = Aeson.String $ show baseCurrency <> "-" <> show quoteCurrency

instance Aeson.FromJSON TradingPair where
  parseJSON = Aeson.withText "TradingPair" $ \text -> do
    let [baseCoin, quoteCoin] = Txt.splitOn "-" text
    case (readMaybe $ toS baseCoin, readMaybe $ toS quoteCoin) of
      (Just baseCurrency, Just quoteCurrency) ->
        pure TradingPair{..}
      _ ->
        Fail.fail $ "Invalid trading pair: " <> toS text

instance Servant.ToHttpApiData TradingPair where
  toUrlPiece TradingPair{..}
    = show baseCurrency <> "-" <> show quoteCurrency


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

instance QC.Arbitrary Candle where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

instance Aeson.FromJSON Candle where
  parseJSON = Aeson.withArray "Candle" $ \array ->
    case (array !? 0, array !? 1, array !? 2, array !? 3, array !? 4) of
      (Just jsonTime, Just jsonLow, Just jsonHigh, Just jsonOpen, Just jsonClose) -> do
        time <-
          Time.posixSecondsToUTCTime <$> Aeson.parseJSON jsonTime

        low   <- Price <$> Aeson.parseJSON jsonLow
        high  <- Price <$> Aeson.parseJSON jsonHigh
        open  <- Price <$> Aeson.parseJSON jsonOpen
        close <- Price <$> Aeson.parseJSON jsonClose

        pure Candle{..}

      _ -> Fail.fail $ "Invalid candle array: " <> show array
