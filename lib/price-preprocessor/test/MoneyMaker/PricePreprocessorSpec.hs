{-# LANGUAGE OverloadedLists #-}

module MoneyMaker.PricePreprocessorSpec
  ( spec
  )
  where

import MoneyMaker.PricePreprocessor.SwingsSpec (mkTime)

import MoneyMaker.PricePreprocessor
import MoneyMaker.PricePreprocessor.ConsolidatedCandles

import qualified MoneyMaker.Coinbase.SDK as Coinbase

import Protolude
import Test.Hspec
import Test.QuickCheck

import qualified Data.Time as Time

spec :: Spec
spec =  do
  describeProcessSingleCandle

  describeGenerateSwingCommands

  describeDeriveGranularity

  describeRoundToGranularity

  -- TODO: describe catchUpWithTheMarket

describeRoundToGranularity :: Spec
describeRoundToGranularity = describe "roundToGranularity" do
  it "is idempotent" $ property \granularity time ->
    roundToGranularity granularity (roundToGranularity granularity time)
      `shouldBe` roundToGranularity granularity time

  it "discards utctDayTime when rounded to OneDay"
    $ property \nonRoundedTime@Time.UTCTime{..} ->
        roundToGranularity OneDay nonRoundedTime
          `shouldBe` Time.UTCTime utctDay 0

  let minute = 60
      hour   = 60 * minute

  it "discards minutes when rounding to hours"
    $ property \utctDay ->
        roundToGranularity OneHour (Time.UTCTime utctDay $ 2 * hour + minute)
          `shouldBe` Time.UTCTime utctDay (2 * hour)

  it "discards seconds when rounding to minute"
    $ property \utctDay ->
        roundToGranularity OneMinute (Time.UTCTime utctDay $ hour + 10 * minute + 23)
          `shouldBe` Time.UTCTime utctDay (hour + 10 * minute)

describeDeriveGranularity :: Spec
describeDeriveGranularity = describe "deriveGranularity" do
  it "returns days when the time difference is more than a day"
    $ property \startTime@Time.UTCTime{..} ((+ 1) . abs -> positiveInteger) ->
        let endTime = Time.UTCTime
              { utctDay = positiveInteger `Time.addDays` utctDay
              , utctDayTime = utctDayTime + 0.001
              }

        in deriveGranularity (Time.diffUTCTime endTime startTime)
             `shouldBe` OneDay

  it "returns hours when the time difference is over an hour but less than a day"
    $ property \startTime ((+ 1) . abs -> numberOfHours) ->
        numberOfHours < 24 ==>
          let endTime
                = (fromInteger numberOfHours * 60 * 60 + 0.001)
                    `Time.addUTCTime ` startTime

          in deriveGranularity (Time.diffUTCTime endTime startTime)
               `shouldBe` OneHour

  it "returns minutes when the time difference is less than an hour"
    $ property \startTime (abs -> numberOfMinutes) ->
        numberOfMinutes <= 60 ==>
          let endTime
                = (fromInteger numberOfMinutes * 60)
                    `Time.addUTCTime ` startTime

          in deriveGranularity (Time.diffUTCTime endTime startTime)
               `shouldBe` OneMinute

describeGenerateSwingCommands :: Spec
describeGenerateSwingCommands = describe "generateSwingCommands" do
  it "returns an error when there are no new candles"
    $ property \error swings ->
        generateSwingCommands error swings NoCandles `shouldBe` Left error

  it "calls processSingleCandle when there is only one new candle"
    $ property \error swings candle ->
        generateSwingCommands error swings (OneCandle candle)
          `shouldBe` Right (processSingleCandle candle swings)

  it "saves ConsolidatedExtremums in order" do
    let newLow  = Coinbase.Price 5
        newHigh = Coinbase.Price 12

        lowFirstExtremums
          = ConsolidatedCandles ConsolidatedExtremums
              { consolidatedLow = TimedPrice
                  { time  = mkTime 5
                  , price = newLow
                  }
              , consolidatedHigh = TimedPrice
                  { time  = mkTime 6
                  , price = newHigh
                  }
              }

        highFirstExtremums
          = ConsolidatedCandles ConsolidatedExtremums
              { consolidatedLow = TimedPrice
                  { time  = mkTime 6
                  , price = newLow
                  }
              , consolidatedHigh = TimedPrice
                  { time  = mkTime 5
                  , price = newHigh
                  }
              }

        saveHigh (mkTime -> time)
          = AddNewPrice TimedPrice{ price = newHigh, time }

        saveLow (mkTime -> time)
          = AddNewPrice TimedPrice{ price = newLow, time }

        error
          = NoNewCandlesFoundError
              { granularity = OneHour
              , productId   = Coinbase.TradingPair Coinbase.BTC Coinbase.USD
              }

    generateSwingCommands error upSwings lowFirstExtremums
      `shouldBe` Right [saveLow 5, saveHigh 6]

    generateSwingCommands error upSwings highFirstExtremums
      `shouldBe` Right [saveHigh 5, saveLow 6]

    generateSwingCommands error downSwings lowFirstExtremums
      `shouldBe` Right [saveLow 5, saveHigh 6]

    generateSwingCommands error downSwings highFirstExtremums
      `shouldBe` Right [saveHigh 5, saveLow 6]

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
