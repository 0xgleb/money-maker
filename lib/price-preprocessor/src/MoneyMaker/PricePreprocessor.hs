{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData      #-}

module MoneyMaker.PricePreprocessor
  ( PriceData
  , preprocessPriceData
  , PreprocessPriceDataErrors
  , TimeOfPreviousSaveIsLaterThanCurrentTimeError(..)

  , swingsAggregateId

  , ExecutionOrders(..)

  , NoNewCandlesFoundError(..)
  , RestrictedGranularity(..)

  , module Swings

  -- Exports for testing:
  , catchUpWithTheMarket
  , CatchUpWithTheMarketErrors

  , deriveGranularity
  , roundToGranularity
  , generateSwingCommands
  , SubCandle(..)
  , processSingleCandle
  )
  where

import MoneyMaker.PricePreprocessor.ConsolidatedCandles
import MoneyMaker.PricePreprocessor.Swings              as Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Error        as Error
import qualified MoneyMaker.Eventful     as Eventful
import           MoneyMaker.MonadPrinter

import Protolude

import qualified Data.Aeson                        as Aeson
import qualified Data.Generics.Product             as Generics
import qualified Data.Time                         as Time
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC

data ExecutionOrders
  = ExecutionOrders
      { message :: LText
      }

data PriceData
  = PriceData
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


type PreprocessPriceDataErrors =
  '[ Eventful.NoEventsFoundError
   , Eventful.CouldntDecodeEventError
   , Coinbase.ServantClientError
   , NoNewCandlesFoundError
   , TimeOfPreviousSaveIsLaterThanCurrentTimeError
   ]

preprocessPriceData
  :: forall errors m
   . ( Eventful.MonadEventStore m
     , Coinbase.CoinbaseRestAPI m
     , PreprocessPriceDataErrors `Error.Elems` errors
     , MonadPrinter m
     )
  => Coinbase.TickerPriceData
  -> m errors PriceData

preprocessPriceData Coinbase.TickerPriceData{ time = currentTime, ..} = do
  savedSwings <-
    Error.catchVoid $ Eventful.getAggregate @SwingEvent swingsAggregateId

  let timeOfPreviousSave
        = getTime $ getLastPrice savedSwings

      timeSinceLastSavedPrice
        = Time.diffUTCTime timeOfPreviousSave currentTime

  swings <-
    if timeSinceLastSavedPrice < 5 * 60 -- if less than 5 minutes passed
    then pure savedSwings
    else catchUpWithTheMarket productId currentTime savedSwings

  pure PriceData{ time = currentTime, ..}


data NoNewCandlesFoundError
  = NoNewCandlesFoundError
      { granularity :: RestrictedGranularity
      , productId   :: Coinbase.TradingPair
      }
  deriving stock (Generic, Show, Eq)

instance QC.Arbitrary NoNewCandlesFoundError where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

data TimeOfPreviousSaveIsLaterThanCurrentTimeError
  = TimeOfPreviousSaveIsLaterThanCurrentTimeError
      { currentTime        :: Time.UTCTime
      , timeOfPreviousSave :: Time.UTCTime
      }
  deriving stock (Generic, Show, Eq)

type CatchUpWithTheMarketErrors =
  '[ Eventful.NoEventsFoundError
   , Eventful.CouldntDecodeEventError
   , Coinbase.ServantClientError
   , NoNewCandlesFoundError
   , TimeOfPreviousSaveIsLaterThanCurrentTimeError
   ]

catchUpWithTheMarket
  :: ( Eventful.MonadEventStore m
     , Coinbase.CoinbaseRestAPI m
     , CatchUpWithTheMarketErrors `Error.Elems` errors
     , MonadPrinter m
     )
  => Coinbase.TradingPair
  -> Time.UTCTime
  -> Swings
  -> m errors Swings

catchUpWithTheMarket productId currentTime savedSwings = do
  let timeOfPreviousSave
        = getTime $ getLastPrice savedSwings

  when (currentTime < timeOfPreviousSave)
    $ Error.throwUltraError
        TimeOfPreviousSaveIsLaterThanCurrentTimeError{..}

  let granularity
        = deriveGranularity $ currentTime `Time.diffUTCTime` timeOfPreviousSave

      startTime
        = roundToGranularity granularity timeOfPreviousSave

      endTime
        = limitTo300Days startTime
        $ roundToGranularity granularity currentTime

  consolidatedCandles <-
    consolidateCandles . fmap Generics.upcast
      <$> Coinbase.getCandles productId
            startTime
            endTime
            (restrictedGranularityToCoinbaseGranularity granularity)

  let swingCommands
        = generateSwingCommands
            NoNewCandlesFoundError{..}
            savedSwings
            consolidatedCandles

  newSwings <- case swingCommands of
    Left error ->
      Error.throwUltraError error

    Right (command :| commands) -> Error.catchVoid do
      swings <- Eventful.applyCommand swingsAggregateId command

      foldM (const $ Eventful.applyCommand swingsAggregateId)
        swings
        commands

  case granularity of
    OneMinute ->
      -- The Coinbase API returns up to 300 candles per request. Because of
      -- deriveGranularity, if we needed more than 300 OneMinute candles
      -- we would request OneHour candles instead. So if the last request
      -- that we did was in minutes then we don't need to get more candles
      pure newSwings

    OneHour ->
      catchUpWithTheMarket productId currentTime newSwings

    OneDay ->
      catchUpWithTheMarket productId currentTime newSwings

  where
    limitTo300Days startTime endTime
      | endTime `Time.diffUTCTime` startTime <= 299 * Time.nominalDay
      = endTime

      | otherwise
      = Time.UTCTime (299 `Time.addDays` Time.utctDay startTime) 0


data RestrictedGranularity
  = OneMinute
  | OneHour
  | OneDay
  deriving stock (Show, Generic, Eq)

instance QC.Arbitrary RestrictedGranularity where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

restrictedGranularityToCoinbaseGranularity
  :: RestrictedGranularity
  -> Coinbase.Granularity
restrictedGranularityToCoinbaseGranularity = \case
  OneMinute -> Coinbase.OneMinute
  OneHour   -> Coinbase.OneHour
  OneDay    -> Coinbase.OneDay

-- granularityToTime :: RestrictedGranularity -> Time.NominalDiffTime
-- granularityToTime = \case
--   OneMinute -> minute
--   OneHour   -> hour
--   OneDay    -> day

--   where
--     minute = 60
--     hour   = 60 * minute
--     day    = 24 * hour


deriveGranularity :: Time.NominalDiffTime -> RestrictedGranularity
deriveGranularity timeDifference
  | timeDifference / day > 1
  = OneDay

  | timeDifference / hour > 1
  = OneHour

  | otherwise
  = OneMinute

  where
    minute = 60
    hour   = 60 * minute
    day    = 24 * hour


roundToGranularity
  :: RestrictedGranularity
  -> Time.UTCTime
  -> Time.UTCTime

roundToGranularity OneDay Time.UTCTime{..}
  = Time.UTCTime utctDay 0

roundToGranularity OneHour Time.UTCTime{..}
  = Time.UTCTime utctDay
  $ (* (60 * 60)) $ fromInteger
  $ fst $ properFraction (utctDayTime / 60 / 60)

roundToGranularity OneMinute Time.UTCTime{..}
  = Time.UTCTime utctDay
  $ (* 60) $ fromInteger
  $ fst $ properFraction (utctDayTime / 60)


generateSwingCommands
  :: NoNewCandlesFoundError
  -> Swings
  -> ConsolidatedCandles
  -> Either NoNewCandlesFoundError (NonEmpty SwingCommand)

generateSwingCommands error savedSwings = \case
  NoCandles ->
    Left error

  OneCandle candle ->
    Right $ processSingleCandle savedSwings $ Generics.upcast candle

  ConsolidatedCandles (lastCandleToCommands -> tailCommands) extremums ->
    Right $ addTailCommands tailCommands
      $ join $ extremums <&> \ConsolidatedExtremums{..} ->
          if getTime consolidatedLow == getTime consolidatedHigh
          then processSingleCandle savedSwings $ SubCandle
                { time = getTime consolidatedLow
                , low  = getPrice consolidatedLow
                , high = getPrice consolidatedHigh
                }

          else
            let (earlierExtremum, laterExtremum) =
                  if getTime consolidatedLow < getTime consolidatedHigh
                  then (consolidatedLow, consolidatedHigh)
                  else (consolidatedHigh, consolidatedLow)

            in [AddNewPrice earlierExtremum, AddNewPrice laterExtremum]

  where
    addTailCommands tailCommands (command :| commands)
      = command :| (commands <> tailCommands)

    lastCandleToCommands :: Maybe Coinbase.Candle -> [SwingCommand]
    lastCandleToCommands = maybe [] \Coinbase.Candle{..} ->
      [ AddNewPrice TimedPrice{ price = open, time } ]

-- | A candle with only high, low, and time (no, open, close, etc)
data SubCandle
  = SubCandle
      { high :: Coinbase.Price
      , low  :: Coinbase.Price
      , time :: Time.UTCTime
      }
  deriving stock (Generic, Show, Eq)

instance QC.Arbitrary SubCandle where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

-- | We need to know the order of highs and lows to construct swings.
-- However, when we have only one new candle, we don't know if the high or the
-- low came first. This function aims to sensibly resolve this problem.
-- You can find an explanation with a diagram in docs/resolve-order-ambiguity.png
processSingleCandle
  :: Swings
  -> SubCandle
  -> NonEmpty SwingCommand

processSingleCandle savedSwings SubCandle{ high = newHigh, low = newLow, ..}
  = case savedSwings of
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
    -- TODO: consider putting in different times so that you can sort by them
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
