{-# LANGUAGE DeriveAnyClass #-}

module MoneyMaker.PricePreprocessor
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)

  , module Swings
  )
  where

import MoneyMaker.PricePreprocessor.Swings as Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Error        as Error
import qualified MoneyMaker.Eventful     as Eventful

import Protolude

import qualified Data.Aeson            as Aeson
-- import qualified Data.Generics.Product as Generics
import qualified Data.Time.Clock       as Time

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { productId :: !Coinbase.TradingPair
      , swings    :: !Swings
      , price     :: !Coinbase.Price
      , time      :: !Time.UTCTime
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

-- data NoNewCandlesFoundAfterAMinuteError
--   = NoNewCandlesFoundAfterAMinuteError

toContractualPriceData
  :: ( Eventful.MonadEventStore m
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     , Eventful.NoEventsFoundError `Error.Elem` errors
     -- , NoNewCandlesFoundAfterAMinuteError `Error.Elem` errors
     , MonadIO (m errors)
     )
  => Coinbase.TickerPriceData
  -> m errors ContractualPriceData
toContractualPriceData Coinbase.TickerPriceData{ time = currentTime, ..} = do
  let id = Eventful.Id [Eventful.uuid|123e4567-e89b-12d3-a456-426614174000|]
      -- just a random ID

  -- savedSwings <-
  swings <-
    Error.catchVoid
      $ Eventful.getAggregate @SwingEvent id

  -- let timeOfPreviousSave
  --       = Generics.getField @"time" $ getLastPrice savedSwings

  --     timeSinceLastSavedPrice
  --       = Time.diffUTCTime timeOfPreviousSave currentTime

  _ <- Error.catchVoid
    $ Eventful.applyCommand id
    $ AddNewPrice price currentTime

  -- swings <-
  --   if timeSinceLastSavedPrice <= 60
  --   then pure savedSwings
  --   else do
  --     maybeConsolidatedCandle <-
  --       consolidateCandles
  --         <$> Coinbase.getCandles productId timeOfPreviousSave currentTime
  --               Coinbase.OneMinute

  --     case maybeConsolidatedCandle of
  --       Nothing ->
  --         Error.throwUltraError NoNewCandlesFoundAfterAMinuteError

  --       Just Coinbase.Candle{..} -> do
  --         what you need to do here is a specific case of what you'll
  --         need to do in the catchUpWithTheMarket function

  --         -- let lastHigh = getLastHigh savedSwings
  --         --     lastLow  = getLastLow  savedSwings

  --         -- when (lastHigh < high && time high)
  --         --   $ Error.catchVoid
  --         --   $ Eventful.applyCommand id
  --         --   $ AddNewPrice high time

  --         -- when (lastLow > low)
  --         --   $ Error.catchVoid
  --         --   $ Eventful.applyCommand id
  --         --   $ AddNewPrice low time

  pure ContractualPriceData{ time = currentTime, ..}



-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: !LText
      }
