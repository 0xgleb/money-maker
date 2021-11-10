{-# LANGUAGE OverloadedLists #-}

module MoneyMaker.PricePreprocessorSpec
  ( spec
  )
  where

import MoneyMaker.PricePreprocessor.SwingsSpec (mkTime)
import MoneyMaker.PricePreprocessor.TestMonad

import MoneyMaker.PricePreprocessor
import MoneyMaker.PricePreprocessor.ConsolidatedCandles

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Error        as Error
import qualified MoneyMaker.Eventful     as Eventful

import Protolude
import Test.Hspec
import Test.QuickCheck

import qualified Data.Generics.Product as Generics
import qualified Data.Time             as Time

spec :: Spec
spec =  do
  describeProcessSingleCandle

  describeGenerateSwingCommands

  describeDeriveGranularity

  describeRoundToGranularity

  describeCatchUpWithTheMarket

describeCatchUpWithTheMarket :: Spec
describeCatchUpWithTheMarket = describe "catchUpWithTheMarket" do
  it "passes the example test case" do
    let initialEvents
          = AddNewPrice TimedPrice
              { time  = Time.UTCTime (Time.fromGregorian 2021 8 30) 0
              , price = Coinbase.Price 48815.00
              }
          & Eventful.handleCommand swingsAggregateId Nothing
          & \case
              Left voidError ->
                absurd voidError

              Right (event :| events) ->
                fmap (Eventful.toStorableEvent swingsAggregateId)
                  $ event : events

    let minute = 60
        hour   = 60 * minute

        (result, logs)
          = runPricePreprocessorMonad @CatchUpWithTheMarketErrors initialEvents do
              void $ Error.catchVoid $ Eventful.applyCommand swingsAggregateId $ AddNewPrice TimedPrice
                { time  = Time.UTCTime (Time.fromGregorian 2021 8 30) 0
                , price = Coinbase.Price 48815.00
                }

              savedSwings <-
                Error.catchVoid (Eventful.getAggregate @SwingEvent swingsAggregateId)

              void $ catchUpWithTheMarket
                  (Coinbase.TradingPair Coinbase.BTC Coinbase.USD)
                  ( Time.UTCTime
                      (Time.fromGregorian 2021 9 12)
                      (17 * hour + 55 * minute + 37) )
                  savedSwings

              Error.catchVoid $ Eventful.getAggregate @SwingEvent swingsAggregateId

        mkTime' day hours minutes
          = Time.UTCTime
              { utctDay     = Time.fromGregorian 2021 9 day
              , utctDayTime = hours * hour + minutes * minute
              }

        expectedSwings
          = SwingDown
                 $ Low  (Coinbase.Price 45913.97) (mkTime' 12 17 55)
          $ Just $ High (Coinbase.Price 46021.38) (mkTime' 12 17 54)
          $ Just $ Low  (Coinbase.Price 45873.98) (mkTime' 12 17 53)
          $ Just $ High (Coinbase.Price 46071.38) (mkTime' 12 17 4)
          $ Just $ Low  (Coinbase.Price 45854.73) (mkTime' 12 17 2)
          $ Just $ High (Coinbase.Price 46095.17) (mkTime' 12 16 0)
          $ Just $ Low  (Coinbase.Price 45792.61) (mkTime' 12 15 0)
          $ Just $ High (Coinbase.Price 48633.50) (mkTime' 12 12 0)
          $ Just $ Low  (Coinbase.Price 41317.00) (mkTime' 12 0  0)
          $ Just $ High (Coinbase.Price 56000.00) (mkTime' 11 0  0)
          $ Just $ Low  (Coinbase.Price 25000.00) (mkTime' 3  0  0)
              Nothing

    putStrLn $ unlines logs

    result `shouldBe` Right expectedSwings


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
  it "returns days when the time difference is more than 2 days"
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
          `shouldBe` Right (processSingleCandle swings $ Generics.upcast candle)

  it "generates the same commands if you turn the commands into candles - without last candle"
    $ property \error swings (ArbitraryExtremums extremums) ->
        case generateSwingCommands error swings (ConsolidatedCandles Nothing extremums) of
          Left _ ->
            expectationFailure "Should only fail when given NoCandles"

          expectedResult@(Right (command :| commands)) ->
            let pretendCandles
                  = (command : commands) <&> \(AddNewPrice TimedPrice{..}) ->
                      Coinbase.Candle time price price price price

            in generateSwingCommands error swings (consolidateCandles pretendCandles)
                 `shouldBe` expectedResult

  -- TODO: test the scenario when there is a last candle
  -- it "generates the same commands if you turn the commands into candles - with last candle"
  --   $ property \error swings lastCandle (ArbitraryExtremums extremums) ->
  --       case generateSwingCommands error swings (ConsolidatedCandles (Just lastCandle) extremums) of
  --         Left _ ->
  --           expectationFailure "Should only fail when given NoCandles"

  --         expectedResult@(Right (command :| commands)) ->
  --           let pretendCandles
  --                 = (command : commands) <&> \(AddNewPrice TimedPrice{..}) ->
  --                     Coinbase.Candle price price price price time

  --           in generateSwingCommands error swings (consolidateCandles pretendCandles)
  --                `shouldBe` expectedResult

  it "saves ConsolidatedExtremums in order" do
    let mkTimedPrice (mkTime -> time) (Coinbase.Price -> price)
          = TimedPrice{..}

        lowFirstExtremums = ConsolidatedCandles Nothing
          [ ConsolidatedExtremums
              { consolidatedLow  = mkTimedPrice 5 5
              , consolidatedHigh = mkTimedPrice 6 12
              }

          , ConsolidatedExtremums
              { consolidatedLow  = mkTimedPrice 8 7
              , consolidatedHigh = mkTimedPrice 7 10
              }
          ]

        highFirstExtremums = ConsolidatedCandles Nothing
          [ ConsolidatedExtremums
              { consolidatedLow  = mkTimedPrice 6 5
              , consolidatedHigh = mkTimedPrice 5 12
              }

          , ConsolidatedExtremums
              { consolidatedLow  = mkTimedPrice 7 7
              , consolidatedHigh = mkTimedPrice 8 10
              }
          ]

        lowFirstExpectedResult = AddNewPrice <$>
          [ mkTimedPrice 5 5
          , mkTimedPrice 6 12
          , mkTimedPrice 7 10
          , mkTimedPrice 8 7
          ]

        highFirstExpectedResult = AddNewPrice <$>
          [ mkTimedPrice 5 12
          , mkTimedPrice 6 5
          , mkTimedPrice 7 7
          , mkTimedPrice 8 10
          ]

        error
          = NoNewCandlesFoundError
              { granularity = OneHour
              , productId   = Coinbase.TradingPair Coinbase.BTC Coinbase.USD
              }


    generateSwingCommands error upSwings lowFirstExtremums
      `shouldBe` Right lowFirstExpectedResult

    generateSwingCommands error upSwings highFirstExtremums
      `shouldBe` Right highFirstExpectedResult

    generateSwingCommands error downSwings lowFirstExtremums
      `shouldBe` Right lowFirstExpectedResult

    generateSwingCommands error downSwings highFirstExtremums
      `shouldBe` Right highFirstExpectedResult


describeProcessSingleCandle :: Spec
describeProcessSingleCandle = describe "processSingleCandle" do
  it "resolves extremum order ambiguity when no extremum is invalidated" do
    let candle = SubCandle
          { time  = mkTime 5
          , high  = Coinbase.Price 7
          , low   = Coinbase.Price 3
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

    processSingleCandle upSwings candle `shouldBe` expectedUpSwingsResult

    processSingleCandle downSwings candle `shouldBe` expectedDownSwingsResult


  it "resolves extremum order ambiguity when previous high is invalidated" do
    let candle = SubCandle
          { time  = mkTime 5
          , high  = Coinbase.Price 9
          , low   = Coinbase.Price 3
          }

        expectedResult
          = pure $ AddNewPrice TimedPrice
              { price = Coinbase.Price 9
              , time  = mkTime 5
              }

    processSingleCandle upSwings candle `shouldBe` expectedResult
    processSingleCandle downSwings candle `shouldBe` expectedResult


  it "resolves extremum order ambiguity when previous low is invalidated" do
    let candle = SubCandle
          { time  = mkTime 5
          , high  = Coinbase.Price 7
          , low   = Coinbase.Price 1
          }

        expectedResult
          = pure $ AddNewPrice TimedPrice
              { price = Coinbase.Price 1
              , time  = mkTime 5
              }

    processSingleCandle upSwings candle `shouldBe` expectedResult
    processSingleCandle downSwings candle `shouldBe` expectedResult

  it "resolves extremum order ambiguity when both extremums are invalidated" do
    let candle = SubCandle
          { time  = mkTime 5
          , high  = Coinbase.Price 9
          , low   = Coinbase.Price 1
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

    processSingleCandle upSwings candle `shouldBe` expectedUpSwingsResult

    processSingleCandle downSwings candle `shouldBe` expectedDownSwingsResult


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
