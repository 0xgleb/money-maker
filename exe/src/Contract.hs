{-# LANGUAGE DeriveAnyClass #-}

module Contract
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)
  , Price(..)
  )
  where

import qualified MoneyMaker.Coinbase.SDK.Websockets as Coinbase
import qualified MoneyMaker.Eventful                as Eventful

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Fixed as Fixed

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { productId :: Coinbase.TradingPair
      , price     :: Text -- TODO: change to a better type for price data
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype Price
  = Price { getPrice :: Fixed.Centi }
  deriving newtype (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

toContractualPriceData
  :: Eventful.MonadEventStore m
  => Coinbase.TickerPriceData
  -> m errors ContractualPriceData
toContractualPriceData Coinbase.TickerPriceData{..} = do
  -- TODO: consider max precision
  pure ContractualPriceData{..}

-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }


-- data SwingCommand
--   = AddNewHigh Price
--   | AddNewLow Price

-- instance Eventful.Command SwingCommand SwingEvent where
--   type CommandErrors SwingCommand = '[]

--   handleCommand pairId


data SwingEvent
  = NewHighReached Price
  | NewLowReached Price
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Swings
  = SwingUp High
  | SwingDown Low

data High
  = High
      { price       :: Price
      , previousLow :: Maybe Low
      }

data Low
  = Low
      { price        :: Price
      , previousHigh :: Maybe High
      }

instance Eventful.Eventful SwingEvent where
  type EventName      SwingEvent = "swing"
  type EventAggregate SwingEvent = Swings
  type EventError     SwingEvent = SwingEvent

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
