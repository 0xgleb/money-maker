{-# LANGUAGE DeriveAnyClass #-}

module Contract
  ( ContractualPriceData
  , toContractualPriceData

  , ContractualPrediction(..)
  )
  where

import qualified MoneyMaker.Coinbase.SDK.Websockets as Coinbase
import qualified MoneyMaker.Eventful                as Eventful

import Protolude

import qualified Data.Aeson as Aeson

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { productId :: Coinbase.TradingPair
      , price     :: Text -- TODO: change to a better type for price data
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

toContractualPriceData
  :: Eventful.MonadEventStore m
  => Coinbase.TickerPriceData
  -> m errors ContractualPriceData
toContractualPriceData Coinbase.TickerPriceData{..} = do
  pure ContractualPriceData{..}

-- Just a placeholder until Python actually sends some useful data
data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }
