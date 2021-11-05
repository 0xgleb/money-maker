{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData      #-}

module MoneyMaker.PricePreprocessor.ConsolidatedCandles
  ( ConsolidatedCandles(..)
  , ConsolidatedExtremums(..)
  , consolidateCandles

  , ArbitraryExtremums(..)
  )
  where

import MoneyMaker.PricePreprocessor.Swings

import qualified MoneyMaker.Coinbase.SDK as Coinbase

import Protolude

import qualified Data.List.NonEmpty as NE
import qualified Prelude
import qualified Protolude.Partial  as Partial
import qualified Test.QuickCheck    as QC

data ConsolidatedCandles
  = NoCandles
  | OneCandle Coinbase.Candle
  | ConsolidatedCandles (Maybe Coinbase.Candle) (NonEmpty ConsolidatedExtremums)
  deriving stock (Eq)

instance Prelude.Show ConsolidatedCandles where
  show = \case
    NoCandles ->
      "No candles"

    OneCandle candle ->
      "One candle: " <> show candle

    ConsolidatedCandles lastCandle (extremum :| extremums)->
      "Unprocessed candle: " <> show lastCandle <> "\n"
        <> foldl (\accumulated next -> accumulated <> "\n" <> show next)
             (show extremum)
             extremums

data ConsolidatedExtremums
  = ConsolidatedExtremums
      { consolidatedLow  :: TimedPrice
      , consolidatedHigh :: TimedPrice
      }
  deriving stock (Generic, Show, Eq)

consolidateCandles :: [Coinbase.Candle] -> ConsolidatedCandles
consolidateCandles (sortOn getTime -> orderedCandles)
  = case foldl consolidateCandle NoCandles orderedCandles of
      consolidated@(ConsolidatedCandles _ (ConsolidatedExtremums{..} :| _)) ->
        let isLater Coinbase.Candle{time}
              =  time > getTime consolidatedLow
              && time > getTime consolidatedHigh

            laterCandles = filter isLater orderedCandles

        in case consolidateCandles laterCandles of
             ConsolidatedCandles lastCandle laterExtremums ->
               ConsolidatedCandles lastCandle
                $ [ConsolidatedExtremums{..}] <> laterExtremums

             OneCandle candle ->
               ConsolidatedCandles (Just candle) [ConsolidatedExtremums{..}]

             NoCandles -> consolidated

      consolidated -> consolidated

  where
    consolidateCandle consolidatedCandle nextCandle
      = case consolidatedCandle of
          NoCandles ->
            OneCandle nextCandle

          OneCandle consolidated ->
            ConsolidatedCandles Nothing $ pure ConsolidatedExtremums
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

          ConsolidatedCandles _ (ConsolidatedExtremums{..} :| _) ->
            ConsolidatedCandles Nothing $ pure ConsolidatedExtremums
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
