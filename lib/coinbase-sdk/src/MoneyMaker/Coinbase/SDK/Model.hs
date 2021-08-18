module MoneyMaker.Coinbase.SDK.Model
  ( Price(..)
  )
  where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Fixed as Fixed

newtype Price
  = Price { getPrice :: Fixed.Centi }
  deriving newtype (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)
