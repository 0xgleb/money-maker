{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData      #-}

module MoneyMaker.PricePreprocessor.ConsolidatedCandles
  ( ConsolidatedCandles(..)
  , ConsolidatedExtremums(..)
  , SubCandle(..)

  , ArbitraryExtremums(..)

  , consolidateCandles
  )
  where

import MoneyMaker.PricePreprocessor.Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase

import Protolude

import qualified Data.List.NonEmpty                as NE
import qualified Data.Time                         as Time
import qualified Protolude.Partial                 as Partial
import qualified Test.QuickCheck                   as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC

data ConsolidatedCandles
  = NoCandles
  | OneCandle SubCandle
  | ConsolidatedCandles (NonEmpty ConsolidatedExtremums)
  deriving stock (Show, Eq)

newtype ArbitraryExtremums
  = ArbitraryExtremums (NonEmpty ConsolidatedExtremums)
  deriving newtype Show

instance QC.Arbitrary ArbitraryExtremums where
  shrink = const []

  arbitrary = do
    timesAndPrices <- QC.arbitrary `QC.suchThat` \(length -> len) ->
      len `mod` 2 == 0 && len >= 2

    let times  = sort $ fst <$> timesAndPrices
        prices = sort $ snd <$> timesAndPrices

    (NE.nonEmpty <$> constructExtremumsList times prices)
      >>= maybe QC.arbitrary (pure . ArbitraryExtremums)

    where
      constructExtremumsList times@(time1:time2:_) prices = do
        isFirst <- QC.chooseAny

        let consolidatedLow
              = TimedPrice (Partial.head prices)
                  if isFirst then time1 else time2

            consolidatedHigh
              = TimedPrice (Partial.last prices)
                  if isFirst then time2 else time1

        fmap (ConsolidatedExtremums{..} :)
          $ constructExtremumsList
              (drop 2 times)
              (Partial.tail $ Partial.init prices)

      constructExtremumsList _ _ = pure []


data ConsolidatedExtremums
  = ConsolidatedExtremums
      { consolidatedLow  :: TimedPrice
      , consolidatedHigh :: TimedPrice
      }
  deriving stock (Generic, Show, Eq)

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

consolidateCandles :: [SubCandle] -> ConsolidatedCandles
consolidateCandles (sortOn getTime -> orderedCandles)
  = case foldl consolidateACandle NoCandles orderedCandles of
      consolidated@(ConsolidatedCandles (ConsolidatedExtremums{..} :| _)) ->
        let isLater SubCandle{time}
              =  time > getTime consolidatedLow
              && time > getTime consolidatedHigh

            laterCandles = filter isLater orderedCandles

        in case consolidateCandles laterCandles of
             ConsolidatedCandles laterExtremums ->
               ConsolidatedCandles
                $ [ConsolidatedExtremums{..}] <> laterExtremums

             _ -> consolidated

      consolidated -> consolidated

  where
    consolidateACandle consolidatedCandle nextCandle
      = case consolidatedCandle of
          NoCandles ->
            OneCandle nextCandle

          OneCandle consolidated ->
            ConsolidatedCandles $ pure ConsolidatedExtremums
              { consolidatedLow =
                  if low nextCandle < low consolidated
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = low nextCandle
                          }
                  else TimedPrice
                          { time  = getTime consolidated
                          , price = low consolidated
                          }

              , consolidatedHigh =
                  if high nextCandle > high consolidated
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = high nextCandle
                          }
                  else TimedPrice
                          { time  = getTime consolidated
                          , price = high consolidated
                          }
              }

          ConsolidatedCandles (ConsolidatedExtremums{..} :| _) ->
            ConsolidatedCandles $ pure ConsolidatedExtremums
              { consolidatedLow =
                  if low nextCandle < getPrice consolidatedLow
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = low nextCandle
                          }
                  else consolidatedLow

              , consolidatedHigh =
                  if high nextCandle > getPrice consolidatedHigh
                  then TimedPrice
                          { time  = getTime nextCandle
                          , price = high nextCandle
                          }
                  else consolidatedHigh
              }
