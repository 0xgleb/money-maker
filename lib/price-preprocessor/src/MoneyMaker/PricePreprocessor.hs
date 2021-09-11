{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData      #-}

module MoneyMaker.PricePreprocessor
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)

  , NoNewCandlesFoundError(..)

  , module Swings

  -- Exports for testing:
  , processSingleCandle
  )
  where

import MoneyMaker.PricePreprocessor.ConsolidatedCandles
import MoneyMaker.PricePreprocessor.Swings              as Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Error        as Error
import qualified MoneyMaker.Eventful     as Eventful

import Protolude

import qualified Data.Aeson      as Aeson
import qualified Data.Time.Clock as Time

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
     , NoNewCandlesFoundError `Error.Elem` errors
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


data NoNewCandlesFoundError
  = NoNewCandlesFoundError
      { granularity :: Coinbase.Granularity
      , productId   :: Coinbase.TradingPair
      }
  deriving stock (Show)

catchUpWithTheMarket
  :: ( Eventful.MonadEventStore m
     , Coinbase.CoinbaseRestAPI m
     , Eventful.NoEventsFoundError `Error.Elem` errors
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     , NoNewCandlesFoundError `Error.Elem` errors
     , Coinbase.ServantClientError `Error.Elem` errors
     , Coinbase.HeaderError `Error.Elem` errors
     )
  => Coinbase.TradingPair
  -> Time.UTCTime
  -> m errors Swings

catchUpWithTheMarket productId currentTime = do
  savedSwings <-
    Error.catchVoid (Eventful.getAggregate @SwingEvent swingsAggregateId)
      -- TODO: cover the case when there are no saved prices
      -- $ Error.catchUltraError @NoEventsFoundError
      --     $ \

  let timeOfPreviousSave
        = getTime $ getLastPrice savedSwings

      granularity
        = deriveGranularity $ Time.diffUTCTime timeOfPreviousSave currentTime

  consolidatedCandles <-
    consolidateCandles
      <$> Coinbase.getCandles productId
            (Just timeOfPreviousSave) -- TODO: round to granularity
            (Just currentTime) -- TODO: round to granularity
            granularity

  -- TODO: add recursion to fully catch up with the market

  let swingCommands
        = generateSwingCommands
            NoNewCandlesFoundError{..}
            savedSwings
            consolidatedCandles

  case swingCommands of
    Left error ->
      Error.throwUltraError error

    Right (command :| commands) -> Error.catchVoid do
      swings <- Eventful.applyCommand swingsAggregateId command

      foldM
        (const $ Eventful.applyCommand swingsAggregateId)
        swings
        commands


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


generateSwingCommands
  :: NoNewCandlesFoundError
  -> Swings
  -> ConsolidatedCandles
  -> Either NoNewCandlesFoundError (NonEmpty SwingCommand)

generateSwingCommands error savedSwings = \case
  NoCandles ->
    Left error

  OneCandle candle ->
    Right $ processSingleCandle candle savedSwings

  ConsolidatedCandles OrderedExtremums{..} ->
    let (earlierExtremum, laterExtremum) =
          if getTime consolidatedLow < getTime consolidatedHigh
          then (consolidatedLow, consolidatedHigh)
          else (consolidatedHigh, consolidatedLow)

    in Right [AddNewPrice earlierExtremum, AddNewPrice laterExtremum]

-- | We need to know the order of highs and lows to construct swings.
-- However, when we have only one new candle, we don't know if the high or the
-- low came first. This function aims to sensibly resolve this problem.
-- You can find an explanation with a diagram in docs/resolve-order-ambiguity.png
processSingleCandle
  :: Coinbase.Candle
  -> Swings
  -> NonEmpty SwingCommand

processSingleCandle Coinbase.Candle{ high = newHigh, low = newLow, ..} = \case
  SwingUp High{ previousLow = Nothing, price = oldHigh } ->
    if newHigh > oldHigh
    then [saveNewHigh, saveNewLow]
    else [saveNewLow , saveNewHigh]

  SwingDown Low{ previousHigh = Nothing, price = oldLow } ->
    if newLow < oldLow
    then [saveNewLow , saveNewHigh]
    else [saveNewHigh, saveNewLow]

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
      = AddNewPrice TimedPrice{ price = newHigh, time }

    saveNewLow
      = AddNewPrice TimedPrice{ price = newLow, time }

    resolveExtremumOrderAmbiguity lastSwingWasUp oldHigh oldLow
      -- Case 1:
      -- Neither the previous high nor the previous low are invalidated.
      -- => Save the local extremum opposite to the previous one.
      -- e.g. if last swing was up then save the new low.

      | oldLow <= newLow && newHigh <= oldHigh && lastSwingWasUp
      = [saveNewLow]

      | oldLow <= newLow && newHigh <= oldHigh && not lastSwingWasUp
      = [saveNewHigh]

      -- Case 2:
      -- One of the previous extremums was invalidated.
      -- => Save the invalidated extremum.
      -- e.g. if new low is lower than the previous one, save it.

      | oldLow <= newLow && newHigh > oldHigh
      = [saveNewHigh]

      | oldLow > newLow && newHigh <= oldHigh
      = [saveNewLow]

      -- Case 3:
      -- Both previous extremums were invalidated.
      -- The new low is lower and the new high is higher.
      --
      -- => Save both new extremums in the order of the previous swing.
      --
      -- e.g. if the previous swing was up then first save the new low
      -- and then the new high.

      | otherwise
      = if lastSwingWasUp
        then [saveNewLow , saveNewHigh]
        else [saveNewHigh, saveNewLow]
