{-# LANGUAGE DeriveAnyClass #-}

module MoneyMaker.PricePreprocessor
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)

  , SwingCommand(..)
  , SwingEvent
  , Swings(..)
  , getLastPrice
  , High(..)
  , Low(..)
  , Price(..)
  )
  where

import qualified MoneyMaker.Coinbase.SDK.Websockets as Coinbase
import qualified MoneyMaker.Error                   as Error
import qualified MoneyMaker.Eventful                as Eventful

import Protolude

import qualified Data.Aeson      as Aeson
import qualified Data.Fixed      as Fixed
import qualified Data.Time.Clock as Time

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { productId :: Coinbase.TradingPair
      , swings    :: Swings
      , price     :: Price
      , time      :: Time.UTCTime
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype Price
  = Price { getPrice :: Fixed.Centi }
  deriving newtype (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

toContractualPriceData
  :: ( Eventful.MonadEventStore m
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     , Eventful.NoEventsFoundError `Error.Elem` errors
     )
  => Coinbase.TickerPriceData
  -> m errors ContractualPriceData
toContractualPriceData Coinbase.TickerPriceData{..} = do
  let id = Eventful.Id [Eventful.uuid|123e4567-e89b-12d3-a456-426614174000|]

  -- maybeSwings <-
  --   Error.catchVoid (Just <$> Eventful.getAggregate @SwingEvent id)
  --     `Error.catchUltraError` \Eventful.NoEventsFoundError -> pure Nothing

  swings <- Error.catchVoid $ Eventful.applyCommand id $ AddNewPrice (Price price) time

  -- swings <- case maybeSwings of
  --   Nothing ->
  --     Error.catchVoid $ Eventful.applyCommand id $ AddNewPrice (Price price) time
  --   Just prevSwings -> do
  --     let lastSavedPrice = getLastPrice prevSwings

  --     if abs (getPrice lastSavedPrice - price) >= 1
  --        then Error.catchVoid $ Eventful.applyCommand id $ AddNewPrice (Price price) time
  --        else pure prevSwings

  -- TODO: consider max precision
  pure ContractualPriceData{ price = Price price, ..}

-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }


data SwingCommand
  = AddNewPrice Price Time.UTCTime

instance Eventful.Command SwingCommand SwingEvent where
  type CommandErrors SwingCommand = '[]

  handleCommand _id Nothing (AddNewPrice price time)
    = pure $ NewLowReached price time :| []

  handleCommand _id (Just swings) (AddNewPrice newPrice time)
    = let previousPrice = case swings of
            SwingUp   High{price} -> price
            SwingDown Low{price}  -> price

      in pure $ if newPrice > previousPrice
            then NewHighReached newPrice time :| []
            else NewLowReached newPrice time :| []


data SwingEvent
  = NewHighReached Price Time.UTCTime
  | NewLowReached Price Time.UTCTime
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Swings
  = SwingUp High
  | SwingDown Low
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

getLastPrice :: Swings -> Price
getLastPrice = \case
  SwingUp High{..}  -> price
  SwingDown Low{..} -> price

data High
  = High
      { price       :: Price
      , time        :: Time.UTCTime
      , previousLow :: Maybe Low
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Low
  = Low
      { price        :: Price
      , time         :: Time.UTCTime
      , previousHigh :: Maybe High
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance Eventful.Eventful SwingEvent where
  type EventName      SwingEvent = "swing"
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


addNewHigh :: Price -> Time.UTCTime -> Low -> High
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

addNewLow :: Price -> Time.UTCTime -> High -> Low
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
