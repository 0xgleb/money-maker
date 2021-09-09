{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module MoneyMaker.PricePreprocessor
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)

  , NoNewCandlesFoundAfterAMinuteError(..)

  , module Swings
  )
  where

import MoneyMaker.PricePreprocessor.Swings as Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Error        as Error
import qualified MoneyMaker.Eventful     as Eventful

import Protolude

import qualified Data.Aeson            as Aeson
import qualified Data.Generics.Product as Generics
import qualified Data.Time.Clock       as Time

-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { productId :: Coinbase.TradingPair
      , swings    :: Swings
      , price     :: Coinbase.Price
      , time      :: Time.UTCTime
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

-- | Just a random ID. Good enough while we use only one trading pair
swingsAggregateId :: Eventful.Id "swings"
swingsAggregateId
  = Eventful.Id [Eventful.uuid|123e4567-e89b-12d3-a456-426614174000|]


toContractualPriceData
  :: ( Eventful.MonadEventStore m
     , Coinbase.CoinbaseRestAPI m
     , Eventful.NoEventsFoundError `Error.Elem` errors
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     , NoNewCandlesFoundAfterAMinuteError `Error.Elem` errors
     , Coinbase.ServantClientError `Error.Elem` errors
     , Coinbase.HeaderError `Error.Elem` errors
     )
  => Coinbase.TickerPriceData
  -> m errors ContractualPriceData

toContractualPriceData Coinbase.TickerPriceData{ time = currentTime, ..} = do
  savedSwings <-
    Error.catchVoid $ Eventful.getAggregate @SwingEvent swingsAggregateId

  let timeOfPreviousSave
        = getTime $ getLastPrice savedSwings

      timeSinceLastSavedPrice
        = Time.diffUTCTime timeOfPreviousSave currentTime

  swings <-
    if timeSinceLastSavedPrice < 5 * 60 -- if less than 5 minutes passed
    then pure savedSwings
    else catchUpWithTheMarket productId currentTime

  pure ContractualPriceData{ time = currentTime, ..}


data NoNewCandlesFoundAfterAMinuteError
  = NoNewCandlesFoundAfterAMinuteError
  deriving stock (Show)

catchUpWithTheMarket
  :: ( Eventful.MonadEventStore m
     , Coinbase.CoinbaseRestAPI m
     , Eventful.NoEventsFoundError `Error.Elem` errors
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     , NoNewCandlesFoundAfterAMinuteError `Error.Elem` errors
     , Coinbase.ServantClientError `Error.Elem` errors
     , Coinbase.HeaderError `Error.Elem` errors
     )
  => Coinbase.TradingPair
  -> Time.UTCTime
  -> m errors Swings

catchUpWithTheMarket productId currentTime = do
  savedSwings <-
    Error.catchVoid (Eventful.getAggregate @SwingEvent swingsAggregateId)
      -- $ Error.catchUltraError @NoEventsFoundError
      --     $ \

  let timeOfPreviousSave
        = getTime $ getLastPrice savedSwings

      timeSinceLastSavedPrice
        = Time.diffUTCTime timeOfPreviousSave currentTime

  consolidatedCandles <-
    consolidateCandles
      <$> Coinbase.getCandles productId
            (Just timeOfPreviousSave) -- TODO: round to granularity
            (Just currentTime) -- TODO: round to granularity
            (deriveGranularity timeSinceLastSavedPrice)

  case (consolidatedCandles, savedSwings) of
    (NoCandles, _) ->
      Error.throwUltraError NoNewCandlesFoundAfterAMinuteError

    (OneCandle candle, _) ->
      processSingleCandle candle savedSwings

    (ConsolidatedCandles OrderedExtremums{..}, _) -> do
      let (earlierExtremum, laterExtremum) =
            if getTime consolidatedLow < getTime consolidatedHigh
            then (consolidatedLow, consolidatedHigh)
            else (consolidatedHigh, consolidatedLow)

      void $ Error.catchVoid
        $ Eventful.applyCommand swingsAggregateId
        $ AddNewPrice earlierExtremum

      Error.catchVoid
        $ Eventful.applyCommand swingsAggregateId
        $ AddNewPrice laterExtremum

deriveGranularity :: Time.NominalDiffTime -> Coinbase.Granularity
deriveGranularity timeDifference
  | timeDifference / day > 1
  = Coinbase.OneDay

  | timeDifference / hour > 1
  = Coinbase.OneHour

  | otherwise
  = Coinbase.OneMinute

  where
    minute = 60
    hour   = 60 * minute
    day    = 24 * hour


processSingleCandle
  :: ( Eventful.MonadEventStore m
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     )
  => Coinbase.Candle
  -> Swings
  -> m errors Swings

processSingleCandle Coinbase.Candle{ high = newHigh, low = newLow, ..} = \case
  SwingUp High{ previousLow = Nothing, price = oldHigh } ->
    if newHigh > oldHigh
    then void saveNewHigh >> saveNewLow
    else void saveNewLow  >> saveNewHigh

  SwingDown Low{ previousHigh = Nothing, price = oldLow } ->
    if newLow < oldLow
    then void saveNewLow  >> saveNewHigh
    else void saveNewHigh >> saveNewLow

  SwingUp previousHigh@High{ previousLow = Just previousLow } ->
    resolveExtremumOrderAmbiguity
      True
      (getPrice previousHigh)
      (getPrice previousLow)

  SwingDown previousLow@Low{ previousHigh = Just previousHigh } ->
    resolveExtremumOrderAmbiguity
      False
      (getPrice previousHigh)
      (getPrice previousLow)

  where
    saveNewHigh
      = Error.catchVoid $ Eventful.applyCommand swingsAggregateId
          $ AddNewPrice TimedPrice{ price = newHigh, time }

    saveNewLow
      = Error.catchVoid $ Eventful.applyCommand swingsAggregateId
          $ AddNewPrice TimedPrice{ price = newLow, time }

    resolveExtremumOrderAmbiguity lastSwingWasUp oldHigh oldLow
      | oldLow <= newLow && newHigh <= oldHigh && lastSwingWasUp
      = saveNewLow

      | oldLow <= newLow && newHigh <= oldHigh && not lastSwingWasUp
      = saveNewHigh

      | oldLow <= newLow && newHigh > oldHigh
      = saveNewHigh

      | oldLow > newLow && newHigh <= oldHigh
      = saveNewLow

      | otherwise -- new low is lower and new high is higher
      = if lastSwingWasUp
        then void saveNewLow  >> saveNewHigh
        else void saveNewHigh >> saveNewLow


data ConsolidatedCandles
  = NoCandles
  | OneCandle Coinbase.Candle
  | ConsolidatedCandles OrderedExtremums

data OrderedExtremums
  = OrderedExtremums
      { consolidatedLow  :: TimedPrice
      , consolidatedHigh :: TimedPrice
      }

consolidateCandles :: [Coinbase.Candle] -> ConsolidatedCandles
consolidateCandles candles
  = foldr consolidateACandle NoCandles $ sortOn getTime candles
  where
    consolidateACandle nextCandle = \case
      NoCandles ->
        OneCandle nextCandle

      OneCandle consolidated ->
        ConsolidatedCandles OrderedExtremums
          { consolidatedLow =
              if Coinbase.low nextCandle < Coinbase.low consolidated
              then TimedPrice
                      { time  = getTime nextCandle
                      , price = Coinbase.low nextCandle
                      }
              else TimedPrice
                      { time  = getTime consolidated
                      , price = Coinbase.low consolidated
                      }

          , consolidatedHigh =
              if Coinbase.high nextCandle > Coinbase.high consolidated
              then TimedPrice
                      { time  = getTime nextCandle
                      , price = Coinbase.high nextCandle
                      }
              else TimedPrice
                      { time  = getTime consolidated
                      , price = Coinbase.high consolidated
                      }
          }

      ConsolidatedCandles OrderedExtremums{..} ->
        ConsolidatedCandles OrderedExtremums
          { consolidatedLow =
              if Coinbase.low nextCandle < getPrice consolidatedLow
              then TimedPrice
                      { time  = getTime nextCandle
                      , price = Coinbase.low nextCandle
                      }
              else consolidatedLow

          , consolidatedHigh =
              if Coinbase.high nextCandle > getPrice consolidatedHigh
              then TimedPrice
                      { time  = getTime nextCandle
                      , price = Coinbase.high nextCandle
                      }
              else consolidatedHigh
          }


getTime
  :: Generics.HasField' "time" mainType fieldType
  => mainType
  -> fieldType
getTime
  = Generics.getField @"time"

getPrice
  :: Generics.HasField' "price" mainType fieldType
  => mainType
  -> fieldType
getPrice
  = Generics.getField @"price"
