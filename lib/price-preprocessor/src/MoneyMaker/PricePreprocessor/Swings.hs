{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module MoneyMaker.PricePreprocessor.Swings
  ( Swings(..)
  , High(..)
  , Low(..)

  , getLastPrice
  , TimedPrice(..)
  , getTime
  , getPrice

  , SwingEvent
  , SwingCommand(..)
  )
  where

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Eventful     as Eventful

import Protolude

import qualified Data.Aeson                        as Aeson
import qualified Data.Generics.Product             as Generics
import qualified Data.Time.Clock                   as Time
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC


data Swings
  = SwingUp High
  | SwingDown Low
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance QC.Arbitrary Swings where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

data High
  = High
      { price       :: Coinbase.Price
      , time        :: Time.UTCTime
      , previousLow :: Maybe Low
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance QC.Arbitrary High where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

data Low
  = Low
      { price        :: Coinbase.Price
      , time         :: Time.UTCTime
      , previousHigh :: Maybe High
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance QC.Arbitrary Low where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

getLastPrice :: Swings -> TimedPrice
getLastPrice = \case
  SwingUp High{..}  -> TimedPrice{..}
  SwingDown Low{..} -> TimedPrice{..}

data TimedPrice
  = TimedPrice
      { price :: Coinbase.Price
      , time  :: Time.UTCTime
      }
  deriving stock (Show, Eq, Generic)

{-# INLINE getTime #-}
getTime
  :: Generics.HasField' "time" mainType fieldType
  => mainType
  -> fieldType
getTime
  = Generics.getField @"time"

{-# INLINE getPrice #-}
getPrice
  :: Generics.HasField' "price" mainType fieldType
  => mainType
  -> fieldType
getPrice
  = Generics.getField @"price"


data SwingEvent
  = NewHighReached Coinbase.Price Time.UTCTime
  | NewLowReached Coinbase.Price Time.UTCTime
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance Eventful.Eventful SwingEvent where
  type EventName      SwingEvent = "swings"
  type EventAggregate SwingEvent = Swings
  type EventError     SwingEvent = Void

  applyEvent Nothing event = pure $ case event of
    NewHighReached price time ->
      SwingUp $ High price time Nothing
    NewLowReached price time ->
      SwingDown $ Low price time Nothing

  applyEvent (Just aggregate) event
    = pure $ case (aggregate, event) of
        ( SwingUp High{previousLow}, NewHighReached price time ) ->
          SwingUp High{..}

        ( SwingDown Low{previousHigh}, NewLowReached price time ) ->
          SwingDown Low{..}

        ( SwingDown low, NewHighReached price time ) ->
          SwingUp $ addNewHigh price time low

        ( SwingUp high, NewLowReached price time ) ->
          SwingDown $ addNewLow price time high


addNewHigh :: Coinbase.Price -> Time.UTCTime -> Low -> High
addNewHigh newHighPrice time low@Low{previousHigh}
  = case previousHigh of
      Nothing ->
        High newHighPrice time $ Just low

      Just High{price = prevHighPrice} | newHighPrice < prevHighPrice ->
        High newHighPrice time $ Just low

      Just high ->
        maybe
          (High newHighPrice time $ Just low)
          (addNewHigh newHighPrice time)
          (previousLow high)

addNewLow :: Coinbase.Price -> Time.UTCTime -> High -> Low
addNewLow newLowPrice time high@High{previousLow}
  = case previousLow of
      Nothing ->
        Low newLowPrice time $ Just high

      Just Low{price = prevLowPrice} | newLowPrice > prevLowPrice ->
        Low newLowPrice time $ Just high

      Just low ->
        maybe
          (Low newLowPrice time $ Just high)
          (addNewLow newLowPrice time)
          (previousHigh low)


data SwingCommand
  = AddNewPrice TimedPrice
  deriving stock (Eq, Show)

instance Eventful.Command SwingCommand SwingEvent where
  type CommandErrors SwingCommand = '[]

  handleCommand _id Nothing (AddNewPrice TimedPrice{..})
    = pure $ NewLowReached price time :| []

  handleCommand _id (Just swings) (AddNewPrice TimedPrice{ price = newPrice, ..})
    = let previousPrice = case swings of
            SwingUp   High{price} -> price
            SwingDown Low{price}  -> price

      in pure $ if newPrice > previousPrice
           then NewHighReached newPrice time :| []
           else NewLowReached newPrice time :| []
