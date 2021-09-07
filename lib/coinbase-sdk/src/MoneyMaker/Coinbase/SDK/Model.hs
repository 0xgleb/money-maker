{-# LANGUAGE StrictData #-}

module MoneyMaker.Coinbase.SDK.Model
  ( Price(..)
  , Currency(..)
  , TradingPair(..)
  )
  where

import Protolude

import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson         as Aeson
import qualified Data.Fixed         as Fixed
import qualified Data.Text          as Txt
import qualified Servant.API        as Servant

-- TODO: this price type is only good for BTC/USD trading pair
-- most coins (that are not USD) can have more than 2 decimal places
newtype Price
  = Price { getPrice :: Fixed.Centi }
  deriving newtype (Show, Read, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

data Currency
  = BTC
  | ETH
  | USD
  | EUR
  deriving stock (Eq, Show, Read)

data TradingPair
  = TradingPair
      { baseCurrency  :: Currency -- ^ BTC in BTC/USD
      , quoteCurrency :: Currency -- ^ USD in BTC/USD
      }
  deriving stock (Eq, Show)

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
