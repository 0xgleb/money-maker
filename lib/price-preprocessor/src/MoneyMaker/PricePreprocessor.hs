{-# LANGUAGE DeriveAnyClass #-}

module MoneyMaker.PricePreprocessor
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)

  , SwingCommand(..)
  , SwingEvent
  , Swings(..)
  , High(..)
  , Low(..)
  , Price(..)
  )
  where

import qualified MoneyMaker.Coinbase.SDK.Websockets as Coinbase
import qualified MoneyMaker.Error                   as Error
import qualified MoneyMaker.Eventful                as Eventful

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Fixed as Fixed

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { productId :: Coinbase.TradingPair
      , swings    :: Swings
      , price     :: Price
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

  maybeSwings <-
    Error.catchVoid (Just <$> Eventful.getAggregate @SwingEvent id)
      `Error.catchUltraError` \Eventful.NoEventsFoundError -> pure Nothing

  swings <- case maybeSwings of
    Nothing ->
      Error.catchVoid $ Eventful.applyCommand id $ AddNewPrice $ Price price
    Just prevSwings -> do
      let lastSavedPrice = getLastPrice prevSwings

      if abs (getPrice lastSavedPrice - price) >= 1
         then Error.catchVoid $ Eventful.applyCommand id $ AddNewPrice $ Price price
         else pure prevSwings

  -- TODO: consider max precision
  pure ContractualPriceData{ price = Price price, ..}

-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }


data SwingCommand
  = AddNewPrice Price

instance Eventful.Command SwingCommand SwingEvent where
  type CommandErrors SwingCommand = '[]

  handleCommand _id Nothing (AddNewPrice price)
    = pure $ NewLowReached price :| []

  handleCommand _id (Just swings) (AddNewPrice newPrice)
    = let previousPrice = case swings of
            SwingUp   High{..} -> price
            SwingDown Low{..}  -> price

      in pure $ if newPrice > previousPrice
            then NewHighReached newPrice :| []
            else NewLowReached newPrice :| []


data SwingEvent
  = NewHighReached Price
  | NewLowReached Price
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
      , previousLow :: Maybe Low
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Low
  = Low
      { price        :: Price
      , previousHigh :: Maybe High
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

instance Eventful.Eventful SwingEvent where
  type EventName      SwingEvent = "swing"
  type EventAggregate SwingEvent = Swings
  type EventError     SwingEvent = Void

  applyEvent Nothing event = pure $ case event of
    NewHighReached price ->
      SwingUp $ High price Nothing
    NewLowReached price ->
      SwingDown $ Low price Nothing

  applyEvent (Just aggregate) event
    = pure $ case (aggregate, event) of
        ( SwingUp High{previousLow}, NewHighReached price ) ->
          SwingUp High{..}

        ( SwingDown Low{previousHigh}, NewLowReached price ) ->
          SwingDown Low{..}

        ( SwingDown low, NewHighReached price ) ->
          SwingUp $ addNewHigh price low

        ( SwingUp high, NewLowReached price ) ->
          SwingDown $ addNewLow price high


addNewHigh :: Price -> Low -> High
addNewHigh newHighPrice low@Low{previousHigh}
  = case previousHigh of
      Nothing ->
        High newHighPrice $ Just low

      Just High{price = prevHighPrice} | newHighPrice < prevHighPrice ->
        High newHighPrice $ Just low

      Just high ->
        maybe
          (High newHighPrice $ Just low)
          (addNewHigh newHighPrice)
          (previousLow high)

addNewLow :: Price -> High -> Low
addNewLow newLowPrice high@High{previousLow}
  = case previousLow of
      Nothing ->
        Low newLowPrice $ Just high

      Just Low{price = prevLowPrice} | newLowPrice > prevLowPrice ->
        Low newLowPrice $ Just high

      Just low ->
        maybe
          (Low newLowPrice $ Just high)
          (addNewLow newLowPrice)
          (previousHigh low)
