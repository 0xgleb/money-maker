{-# LANGUAGE OverloadedLists #-}

module MoneyMaker.PricePreprocessorSpec
  ( spec
  )
  where

import MoneyMaker.PricePreprocessor.SwingsSpec (mkTime)

import MoneyMaker.PricePreprocessor

import qualified MoneyMaker.Coinbase.SDK as Coinbase

import Protolude
import Test.Hspec

spec :: Spec
spec =  do
  describeProcessSingleCandle

  _describeCatchUpWithTheMarket

describeProcessSingleCandle :: Spec
describeProcessSingleCandle = describe "processSingleCandle" do
  it "resolves extremum order ambiguity when no extremum is invalidated" do
    let candle = Coinbase.Candle
          { time  = mkTime 5
          , high  = Coinbase.Price 7
          , low   = Coinbase.Price 3
          , open  = Coinbase.Price 4
          , close = Coinbase.Price 5
          }

        expectedUpSwingsResult
          = pure $ AddNewPrice TimedPrice
              { price = Coinbase.Price 3
              , time  = mkTime 5
              }

        expectedDownSwingsResult
          = pure $ AddNewPrice TimedPrice
              { price = Coinbase.Price 7
              , time  = mkTime 5
              }

    processSingleCandle candle upSwings `shouldBe` expectedUpSwingsResult

    processSingleCandle candle downSwings `shouldBe` expectedDownSwingsResult


  it "resolves extremum order ambiguity when previous high is invalidated" do
    let candle = Coinbase.Candle
          { time  = mkTime 5
          , high  = Coinbase.Price 9
          , low   = Coinbase.Price 3
          , open  = Coinbase.Price 4
          , close = Coinbase.Price 5
          }

        expectedResult
          = pure $ AddNewPrice TimedPrice
              { price = Coinbase.Price 9
              , time  = mkTime 5
              }

    processSingleCandle candle upSwings `shouldBe` expectedResult
    processSingleCandle candle downSwings `shouldBe` expectedResult


  it "resolves extremum order ambiguity when previous low is invalidated" do
    let candle = Coinbase.Candle
          { time  = mkTime 5
          , high  = Coinbase.Price 7
          , low   = Coinbase.Price 1
          , open  = Coinbase.Price 4
          , close = Coinbase.Price 5
          }

        expectedResult
          = pure $ AddNewPrice TimedPrice
              { price = Coinbase.Price 1
              , time  = mkTime 5
              }

    processSingleCandle candle upSwings `shouldBe` expectedResult
    processSingleCandle candle downSwings `shouldBe` expectedResult

  it "resolves extremum order ambiguity when both extremums are invalidated" do
    let candle = Coinbase.Candle
          { time  = mkTime 5
          , high  = Coinbase.Price 9
          , low   = Coinbase.Price 1
          , open  = Coinbase.Price 4
          , close = Coinbase.Price 5
          }

        expectedUpSwingsResult =
          [ AddNewPrice TimedPrice
              { price = Coinbase.Price 1
              , time  = mkTime 5
              }

          , AddNewPrice TimedPrice
              { price = Coinbase.Price 9
              , time  = mkTime 5
              }
          ]

        expectedDownSwingsResult =
          [ AddNewPrice TimedPrice
              { price = Coinbase.Price 9
              , time  = mkTime 5
              }

          , AddNewPrice TimedPrice
              { price = Coinbase.Price 1
              , time  = mkTime 5
              }
          ]

    processSingleCandle candle upSwings `shouldBe` expectedUpSwingsResult

    processSingleCandle candle downSwings `shouldBe` expectedDownSwingsResult


upSwings :: Swings
upSwings
  = SwingUp
  $ High (Coinbase.Price 7.5) (mkTime 4)
  $ Just $ Low (Coinbase.Price 2.5) (mkTime 3)
  $ Just $ High (Coinbase.Price 10) (mkTime 2)
  $ Just $ Low (Coinbase.Price 0) (mkTime 1) Nothing

downSwings :: Swings
downSwings
  = SwingDown
  $ Low (Coinbase.Price 2.5) (mkTime 3)
  $ Just $ High (Coinbase.Price 7.5) (mkTime 4)
  $ Just $ Low (Coinbase.Price 0) (mkTime 1)
  $ Just $ High (Coinbase.Price 10) (mkTime 2) Nothing
