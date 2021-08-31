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
import qualified Data.Generics.Product as Generics
import qualified Data.Time.Clock       as Time

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

toContractualPriceData
  :: ( Eventful.MonadEventStore m
     , Eventful.CouldntDecodeEventError `Error.Elem` errors
     , Eventful.NoEventsFoundError `Error.Elem` errors
     , MonadIO (m errors)
     )
  => Coinbase.TickerPriceData
  -> m errors ContractualPriceData
toContractualPriceData Coinbase.TickerPriceData{..} = do
  let id = Eventful.Id [Eventful.uuid|123e4567-e89b-12d3-a456-426614174000|]
      -- just a random ID

  currentTime <- liftIO Time.getCurrentTime

  savedSwings <-
    Error.catchVoid
      $ Eventful.getAggregate @SwingEvent id

  let timeOfPreviousSave
        = Generics.getField @"time" $ getLastPrice savedSwings

      timeSinceLastSavedPrice
        = Time.diffUTCTime timeOfPreviousSave currentTime

  swings <-
    if timeSinceLastSavedPrice <= 60
    then pure savedSwings
    else do
      candles <-
        Coinbase.getCandles productId timeOfPreviousSave currentTime
          Coinbase.OneMinute

      convertCandlesToPriceUpdates

      -- Error.catchVoid
      --   $ Eventful.applyCommand id
      --   $ AddNewPrice (Coinbase.Price price) time

  pure ContractualPriceData{ price = Coinbase.Price price, ..}



-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }
