{-# LANGUAGE StrictData #-}

module MoneyMaker.PricePreprocessor.ConsolidatedCandles
  ( ConsolidatedCandles(..)
  , OrderedExtremums(..)

  , consolidateCandles
  )
  where

import MoneyMaker.PricePreprocessor.Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase

import Protolude

data ConsolidatedCandles
  = NoCandles
  | OneCandle Coinbase.Candle
  | ConsolidatedCandles OrderedExtremums
  deriving stock (Show, Eq)

data OrderedExtremums
  = OrderedExtremums
      { consolidatedLow  :: TimedPrice
      , consolidatedHigh :: TimedPrice
      }
  deriving stock (Show, Eq)

consolidateCandles :: [Coinbase.Candle] -> ConsolidatedCandles
consolidateCandles candles
  = foldl consolidateACandle NoCandles $ sortOn getTime candles
  where
    consolidateACandle consolidatedCandle nextCandle
      = case consolidatedCandle of
          NoCandles ->
            OneCandle nextCandle

          OneCandle consolidated ->
            ConsolidatedCandles OrderedExtremums
              { consolidatedLow =
                  if Coinbase.low nextCandle < Coinbase.low consolidated
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = Coinbase.low nextCandle
                          }
                  else TimedPrice
                          { time  = getTime consolidated
                          , price = Coinbase.low consolidated
                          }

              , consolidatedHigh =
                  if Coinbase.high nextCandle > Coinbase.high consolidated
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = Coinbase.high nextCandle
                          }
                  else TimedPrice
                          { time  = getTime consolidated
                          , price = Coinbase.high consolidated
                          }
              }

          ConsolidatedCandles OrderedExtremums{..} ->
            ConsolidatedCandles OrderedExtremums
              { consolidatedLow =
                  if Coinbase.low nextCandle < getPrice consolidatedLow
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = Coinbase.low nextCandle
                          }
                  else consolidatedLow

              , consolidatedHigh =
                  if Coinbase.high nextCandle > getPrice consolidatedHigh
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = Coinbase.high nextCandle
                          }
                  else consolidatedHigh
              }
