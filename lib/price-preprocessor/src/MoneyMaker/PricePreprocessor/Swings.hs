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

import qualified Prelude
import qualified Data.Aeson                        as Aeson
import qualified Data.Generics.Product             as Generics
import qualified Data.Time.Clock                   as Time
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC


data Swings
  = SwingUp High
  | SwingDown Low
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance QC.Arbitrary Swings where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

instance Prelude.Show Swings where
  show = ("\n" <>) . \case
    SwingUp high  -> show high
    SwingDown low -> show low

data High
  = High
      { price       :: Coinbase.Price
      , time        :: Time.UTCTime
      , previousLow :: Maybe Low
      }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance QC.Arbitrary High where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

instance Prelude.Show High where
  show High{..}
    = "HIGH: "
    <> show price
    <> " at "
    <> show time
    <> "\n"
    <> fromMaybe "" (show <$> previousLow)

data Low
  = Low
      { price        :: Coinbase.Price
      , time         :: Time.UTCTime
      , previousHigh :: Maybe High
      }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance Prelude.Show Low where
  show Low{..}
    = "LOW:  "
    <> show price
    <> " at "
    <> show time
    <> "\n"
    <> fromMaybe "" (show <$> previousHigh)

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
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance QC.Arbitrary TimedPrice where
  arbitrary = QC.genericArbitrary
  shrink    = QC.genericShrink

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
  = NewHighReached TimedPrice
  | NewLowReached TimedPrice
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance Eventful.Eventful SwingEvent where
  type EventName      SwingEvent = "swings"
  type EventAggregate SwingEvent = Swings
  type EventError     SwingEvent = Void

  applyEvent Nothing event = pure $ case event of
    NewHighReached TimedPrice{..} ->
      SwingUp $ High price time Nothing
    NewLowReached TimedPrice{..} ->
      SwingDown $ Low price time Nothing

  applyEvent (Just aggregate) event
    = pure $ case (aggregate, event) of
        ( SwingUp High{previousLow}, NewHighReached TimedPrice{..} ) ->
          SwingUp High{..}

        ( SwingDown Low{previousHigh}, NewLowReached TimedPrice{..} ) ->
          SwingDown Low{..}

        ( SwingDown low, NewHighReached TimedPrice{..} ) ->
          SwingUp $ addNewHigh price time low

        ( SwingUp high, NewLowReached TimedPrice{..} ) ->
          SwingDown $ addNewLow price time high


addNewHigh :: Coinbase.Price -> Time.UTCTime -> Low -> High
addNewHigh newHighPrice time low@Low{previousHigh}
  = case previousHigh of
      Nothing ->
        High newHighPrice time $ Just low

      Just High{price = prevHighPrice} | newHighPrice <= prevHighPrice ->
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

      Just Low{price = prevLowPrice} | newLowPrice >= prevLowPrice ->
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
  type CommandError SwingCommand = Void

  handleCommand _id Nothing (AddNewPrice TimedPrice{..})
    = pure $ NewLowReached TimedPrice{..} :| []

  handleCommand _id (Just swings) (AddNewPrice TimedPrice{ price = newPrice, ..})
    = let previousPrice = case swings of
            SwingUp   High{price} -> price
            SwingDown Low{price}  -> price

      in pure
           if newPrice > previousPrice
           then NewHighReached TimedPrice{ price = newPrice, time } :| []
           else NewLowReached TimedPrice{ price = newPrice, time } :| []
