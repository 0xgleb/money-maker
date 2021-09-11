module MoneyMaker.PricePreprocessor.ConsolidatedCandlesSpec
  ( spec
  )
  where

import MoneyMaker.PricePreprocessor.Swings
import MoneyMaker.PricePreprocessor.SwingsSpec (mkTime)

import MoneyMaker.PricePreprocessor.ConsolidatedCandles

import qualified MoneyMaker.Coinbase.SDK as Coinbase

import Protolude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "consolidateCandles" do
  it "always returns OrderedExtremums{..} when there are at least 2 candles"
    $ property \candle anotherCandle candles ->
        case consolidateCandles (candle : anotherCandle : candles) of
          NoCandles             -> False
          OneCandle _           -> False
          ConsolidatedCandles _ -> True

  it "always returns the candle given to it when there is only 1 candle"
    $ property \candle ->
        consolidateCandles [candle] `shouldBe` OneCandle candle

  it "returns NoCandles when given an empty list" do
    consolidateCandles [] `shouldBe` NoCandles

  it "correctly extracts the high and low" do
    let candles =
          [ Coinbase.Candle
              { time  = mkTime 1
              , low   = Coinbase.Price 2
              , high  = Coinbase.Price 4
              , open  = Coinbase.Price 3
              , close = Coinbase.Price 3
              }

          , Coinbase.Candle
              { time  = mkTime 2
              , low   = Coinbase.Price 1
              , high  = Coinbase.Price 3
              , open  = Coinbase.Price 2
              , close = Coinbase.Price 2
              }

          , Coinbase.Candle
              { time  = mkTime 3
              , low   = Coinbase.Price 2
              , high  = Coinbase.Price 3
              , open  = Coinbase.Price 2
              , close = Coinbase.Price 2
              }

          , Coinbase.Candle
              { time  = mkTime 4
              , low   = Coinbase.Price 2
              , high  = Coinbase.Price 4
              , open  = Coinbase.Price 2
              , close = Coinbase.Price 2
              }
          ]

        expectedResult
          = ConsolidatedCandles OrderedExtremums
              { consolidatedLow = TimedPrice
                  { time  = mkTime 2
                  , price = Coinbase.Price 1
                  }

              , consolidatedHigh = TimedPrice
                  { time  = mkTime 1
                  , price = Coinbase.Price 4
                  }
              }

    consolidateCandles candles `shouldBe` expectedResult

  it "gives the same result regardless of the order of the items" do
    let earlierCandle
          = Coinbase.Candle
              { time  = mkTime 1
              , low   = Coinbase.Price 1
              , high  = Coinbase.Price 3
              , open  = Coinbase.Price 2
              , close = Coinbase.Price 2
              }

        laterCandle
          = Coinbase.Candle
              { time  = mkTime 2
              , low   = Coinbase.Price 1
              , high  = Coinbase.Price 5
              , open  = Coinbase.Price 2
              , close = Coinbase.Price 3
              }

        expectedResult
          = ConsolidatedCandles OrderedExtremums
              { consolidatedLow = TimedPrice
                  { price = Coinbase.Price 1
                  , time  = mkTime 1
                  }
              , consolidatedHigh = TimedPrice
                  { price = Coinbase.Price 5
                  , time  = mkTime 2
                  }
              }

    consolidateCandles [earlierCandle, laterCandle] `shouldBe` expectedResult
    consolidateCandles [laterCandle, earlierCandle] `shouldBe` expectedResult
