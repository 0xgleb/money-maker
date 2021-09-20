{-# LANGUAGE OverloadedLists #-}

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
  it "always returns ConsolidatedExtremums{..} when there are at least 2 candles"
    $ property \candle anotherCandle candles ->
        case consolidateCandles (candle : anotherCandle : candles) of
          NoCandles               -> False
          OneCandle _             -> False
          ConsolidatedCandles _ _ -> True

  it "always returns the candle given to it when there is only 1 candle"
    $ property \candle ->
        consolidateCandles [candle] == OneCandle candle

  it "returns NoCandles when given an empty list" do
    consolidateCandles [] `shouldBe` NoCandles

  it "correctly extracts the highs and lows" do
    let candles =
          [ Coinbase.Candle
              { time  = mkTime 1
              , open  = Coinbase.Price 3
              , low   = Coinbase.Price 2
              , high  = Coinbase.Price 4
              , close = Coinbase.Price 3
              }

          , Coinbase.Candle
              { time  = mkTime 2
              , open  = Coinbase.Price 2
              , low   = Coinbase.Price 1
              , high  = Coinbase.Price 3
              , close = Coinbase.Price 2
              }

          , Coinbase.Candle
              { time  = mkTime 3
              , open  = Coinbase.Price 2
              , low   = Coinbase.Price 2
              , high  = Coinbase.Price 3
              , close = Coinbase.Price 2
              }

          , Coinbase.Candle
              { time  = mkTime 4
              , open  = Coinbase.Price 3
              , low   = Coinbase.Price 3
              , high  = Coinbase.Price 4
              , close = Coinbase.Price 4
              }
          ]

        expectedResult = ConsolidatedCandles Nothing
          [ ConsolidatedExtremums
              { consolidatedHigh = TimedPrice
                  { time  = mkTime 1
                  , price = Coinbase.Price 4
                  }

              , consolidatedLow = TimedPrice
                  { time  = mkTime 2
                  , price = Coinbase.Price 1
                  }
              }

          , ConsolidatedExtremums
              { consolidatedLow = TimedPrice
                  { time  = mkTime 3
                  , price = Coinbase.Price 2
                  }

              , consolidatedHigh = TimedPrice
                  { time  = mkTime 4
                  , price = Coinbase.Price 4
                  }
              }
          ]

    consolidateCandles candles `shouldBe` expectedResult

  it "gives the same result regardless of the order of the items" do
    let candles =
          [ Coinbase.Candle
              { time  = mkTime 1
              , open  = Coinbase.Price 15
              , low   = Coinbase.Price 1
              , high  = Coinbase.Price 30
              , close = Coinbase.Price 15
              }

          , Coinbase.Candle
              { time  = mkTime 2
              , open  = Coinbase.Price 25
              , low   = Coinbase.Price 1
              , high  = Coinbase.Price 50
              , close = Coinbase.Price 25
              }

          , Coinbase.Candle
              { time  = mkTime 3
              , open  = Coinbase.Price 20
              , low   = Coinbase.Price 10
              , high  = Coinbase.Price 30
              , close = Coinbase.Price 20
              }

          , Coinbase.Candle
              { time  = mkTime 4
              , open  = Coinbase.Price 30
              , low   = Coinbase.Price 20
              , high  = Coinbase.Price 40
              , close = Coinbase.Price 30
              }

          , lastCandle
          ]

        lastCandle
          = Coinbase.Candle
              { time  = mkTime 5
              , open  = Coinbase.Price 30
              , low   = Coinbase.Price 30
              , high  = Coinbase.Price 30
              , close = Coinbase.Price 30
              }

        expectedResult = ConsolidatedCandles (Just lastCandle)
          [ ConsolidatedExtremums
              { consolidatedLow = TimedPrice
                  { price = Coinbase.Price 1
                  , time  = mkTime 1
                  }
              , consolidatedHigh = TimedPrice
                  { price = Coinbase.Price 50
                  , time  = mkTime 2
                  }
              }

          , ConsolidatedExtremums
              { consolidatedLow = TimedPrice
                  { price = Coinbase.Price 10
                  , time  = mkTime 3
                  }
              , consolidatedHigh = TimedPrice
                  { price = Coinbase.Price 40
                  , time  = mkTime 4
                  }
              }
          ]

    consolidateCandles candles `shouldBe` expectedResult
    consolidateCandles (reverse candles) `shouldBe` expectedResult
