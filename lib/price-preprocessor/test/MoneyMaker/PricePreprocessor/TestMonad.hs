{-# LANGUAGE UndecidableInstances #-}

module MoneyMaker.PricePreprocessor.TestMonad
  ( PricePreprocessorTestMonad
  , runPricePreprocessorMonad
  , sampleMinuteCandles
  )
  where

import qualified MoneyMaker.Coinbase.SDK as Coinbase
import qualified MoneyMaker.Eventful     as Eventful

import MoneyMaker.Based

import qualified Data.ByteString.Char8 as BS
import qualified Data.Time             as Time

{-
import qualified Control.Monad.Logger    as Logger
import qualified Data.Pool               as Pool
import qualified Database.Persist.Sql    as Persist
import qualified Database.Persist.Sqlite as Sqlite
import qualified System.IO.Unsafe        as Unsafe

newtype PricePreprocessorTestMonad errors a
  = PricePreprocessorTestMonad
      { runPricePreprocessorTestMonad
          :: Eventful.SqlEventStoreT (StateT [Text] (Logger.NoLoggingT IO)) errors a
      }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUltraError
    , Eventful.MonadEventStore
    )

instance MonadPrinter PricePreprocessorTestMonad where
  say text
    = PricePreprocessorTestMonad
    $ Eventful.SqlEventStoreT
    $ lift
    $ liftToUltraExceptT
    $ state $ ((),) . (<> [text])

runPricePreprocessorMonad
  :: forall errors a
   . [Eventful.StorableEvent]
  -> PricePreprocessorTestMonad errors a
  -> (Either (OneOf errors) a, [Text])

runPricePreprocessorMonad _initialEvents procedure
  = Unsafe.unsafePerformIO $ Logger.runNoLoggingT $ Sqlite.withSqlitePool ":memory:" 1 $ \connectionPool -> do
      Pool.withResource connectionPool
        $ runReaderT $ Persist.runMigration Eventful.migrateAll

      flip runStateT [] $ Eventful.runSqlEventStoreT connectionPool
        $ runPricePreprocessorTestMonad procedure

-}

newtype PricePreprocessorTestMonad errors a
  = PricePreprocessorTestMonad
      { runPricePreprocessorTestMonad
          :: Eventful.InMemoryEventStoreT (State [Text]) errors a
      }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadUltraError
    , Eventful.MonadEventStore
    )

instance {-# OVERLAPPING #-} MonadPrinter PricePreprocessorTestMonad where
  say text
    = PricePreprocessorTestMonad
    $ Eventful.InMemoryEventStoreT
    $ lift
    $ liftToUltraExceptT
    $ state $ ((),) . (<> [text])

runPricePreprocessorMonad
  :: forall errors a
   . [Eventful.StorableEvent]
  -> PricePreprocessorTestMonad errors a
  -> (Either (OneOf errors) a, [Text])

runPricePreprocessorMonad initialEvents procedure
  = first (fmap fst) $ flip runState []
  $ Eventful.runInMemoryEventStoreT initialEvents
  $ runPricePreprocessorTestMonad procedure

instance {-# OVERLAPPING #-} Coinbase.CoinbaseRestAPI PricePreprocessorTestMonad
  where
    getCandles tradingPair startTime endTime granularity
      | startTime == Time.UTCTime (Time.fromGregorian 2021 8 30) 0
          && endTime == Time.UTCTime (Time.fromGregorian 2021 9 12) 0
          && granularity == Coinbase.OneDay
          && tradingPair == Coinbase.TradingPair Coinbase.BTC Coinbase.USD
      = pure sampleDailyCandles

      | startTime == Time.UTCTime (Time.fromGregorian 2021 9 12) 0
          && endTime == Time.UTCTime (Time.fromGregorian 2021 9 12) (17 * 60 * 60)
          && granularity == Coinbase.OneHour
          && tradingPair == Coinbase.TradingPair Coinbase.BTC Coinbase.USD
      = pure sampleHourlyCandles

      | startTime == Time.UTCTime (Time.fromGregorian 2021 9 12) (17 * 60 * 60)
          && endTime == Time.UTCTime (Time.fromGregorian 2021 9 12) (17 * 60 * 60 + 55 * 60)
          && granularity == Coinbase.OneMinute
          && tradingPair == Coinbase.TradingPair Coinbase.BTC Coinbase.USD
      = pure sampleMinuteCandles

      | otherwise
      = throwUltraError
      $ Coinbase.FailureResponse $ "This example is not supported"
      <> " tradingPair = " <> bsShow tradingPair
      <> " startTime = " <> bsShow startTime
      <> " endTime = " <> bsShow endTime
      <> " granularity = " <> bsShow granularity
      where
        bsShow = BS.pack . show

sampleDailyCandles :: [Coinbase.Candle]
sampleDailyCandles =
  [ Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 12) 0
      , low   = Coinbase.Price 41317.00
      , high  = Coinbase.Price 48633.50
      , open  = Coinbase.Price 45330.00
      , close = Coinbase.Price 46056.44
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 11) 0
      , low   = Coinbase.Price 40784.00
      , high  = Coinbase.Price 56000.00
      , open  = Coinbase.Price 44847.87
      , close = Coinbase.Price 45337.23
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 10) 0
      , low   = Coinbase.Price 38113.00
      , high  = Coinbase.Price 55999.00
      , open  = Coinbase.Price 48872.00
      , close = Coinbase.Price 44846.40
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 9 ) 0
      , low   = Coinbase.Price 38678.00
      , high  = Coinbase.Price 51089.59
      , open  = Coinbase.Price 46083.77
      , close = Coinbase.Price 47679.52
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 8 ) 0
      , low   = Coinbase.Price 44480.97
      , high  = Coinbase.Price 47359.51
      , open  = Coinbase.Price 46883.84
      , close = Coinbase.Price 46083.75
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 7 ) 0
      , low   = Coinbase.Price 42716.00
      , high  = Coinbase.Price 55000.00
      , open  = Coinbase.Price 52710.67
      , close = Coinbase.Price 46810.74
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 6 ) 0
      , low   = Coinbase.Price 50943.00
      , high  = Coinbase.Price 53000.00
      , open  = Coinbase.Price 51783.29
      , close = Coinbase.Price 52710.67
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 5 ) 0
      , low   = Coinbase.Price 49514.35
      , high  = Coinbase.Price 51933.51
      , open  = Coinbase.Price 50000.00
      , close = Coinbase.Price 51783.29
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 4 ) 0
      , low   = Coinbase.Price 50000.00
      , high  = Coinbase.Price 50549.71
      , open  = Coinbase.Price 50020.50
      , close = Coinbase.Price 50000.00
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 3 ) 0
      , low   = Coinbase.Price 25000.00
      , high  = Coinbase.Price 50249.96
      , open  = Coinbase.Price 49300.56
      , close = Coinbase.Price 50020.48
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 2 ) 0
      , low   = Coinbase.Price 36769.79
      , high  = Coinbase.Price 50000.00
      , open  = Coinbase.Price 48836.21
      , close = Coinbase.Price 49300.54
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 9 1 ) 0
      , low   = Coinbase.Price 46541.85
      , high  = Coinbase.Price 49656.49
      , open  = Coinbase.Price 47133.76
      , close = Coinbase.Price 48836.19
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 8 31) 0
      , low   = Coinbase.Price 43000.00
      , high  = Coinbase.Price 49360.12
      , open  = Coinbase.Price 46979.50
      , close = Coinbase.Price 47133.74
      }
  , Coinbase.Candle
      { time  = Time.UTCTime (Time.fromGregorian 2021 8 30) 0
      , low   = Coinbase.Price 46896.50
      , high  = Coinbase.Price 49223.81
      , open  = Coinbase.Price 48815.00
      , close = Coinbase.Price 46979.48
      }
  ]

sampleHourlyCandles :: [Coinbase.Candle]
sampleHourlyCandles =
  [ Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 17
      , low   = Coinbase.Price 45854.73
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45857.43
      , close = Coinbase.Price 45924.99
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 16
      , low   = Coinbase.Price 45841.38
      , high  = Coinbase.Price 46095.17
      , open  = Coinbase.Price 45841.38
      , close = Coinbase.Price 45874.27
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 15
      , low   = Coinbase.Price 45792.61
      , high  = Coinbase.Price 47008.32
      , open  = Coinbase.Price 45888.10
      , close = Coinbase.Price 45816.73
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 14
      , low   = Coinbase.Price 45832.46
      , high  = Coinbase.Price 47008.32
      , open  = Coinbase.Price 45943.84
      , close = Coinbase.Price 45888.12
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 13
      , low   = Coinbase.Price 45823.81
      , high  = Coinbase.Price 47008.32
      , open  = Coinbase.Price 46020.41
      , close = Coinbase.Price 45943.86
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 12
      , low   = Coinbase.Price 45965.88
      , high  = Coinbase.Price 48633.50
      , open  = Coinbase.Price 46085.65
      , close = Coinbase.Price 46020.43
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 11
      , low   = Coinbase.Price 45800.00
      , high  = Coinbase.Price 46815.42
      , open  = Coinbase.Price 45990.91
      , close = Coinbase.Price 46085.66
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 10
      , low   = Coinbase.Price 45795.32
      , high  = Coinbase.Price 46815.42
      , open  = Coinbase.Price 45864.00
      , close = Coinbase.Price 45990.91
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 9
      , low   = Coinbase.Price 45800.00
      , high  = Coinbase.Price 46815.42
      , open  = Coinbase.Price 46127.29
      , close = Coinbase.Price 45864.00
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 8
      , low   = Coinbase.Price 45316.12
      , high  = Coinbase.Price 46133.64
      , open  = Coinbase.Price 45366.74
      , close = Coinbase.Price 46133.62
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 7
      , low   = Coinbase.Price 44146.98
      , high  = Coinbase.Price 45391.49
      , open  = Coinbase.Price 45166.89
      , close = Coinbase.Price 45366.72
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 6
      , low   = Coinbase.Price 44648.00
      , high  = Coinbase.Price 45423.15
      , open  = Coinbase.Price 45390.00
      , close = Coinbase.Price 45166.87
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 5
      , low   = Coinbase.Price 45289.67
      , high  = Coinbase.Price 45495.29
      , open  = Coinbase.Price 45340.45
      , close = Coinbase.Price 45398.39
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 4
      , low   = Coinbase.Price 45235.64
      , high  = Coinbase.Price 45501.37
      , open  = Coinbase.Price 45310.00
      , close = Coinbase.Price 45337.97
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 3
      , low   = Coinbase.Price 41397.00
      , high  = Coinbase.Price 45348.19
      , open  = Coinbase.Price 45318.25
      , close = Coinbase.Price 45317.75
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 2
      , low   = Coinbase.Price 44729.00
      , high  = Coinbase.Price 45701.37
      , open  = Coinbase.Price 44955.85
      , close = Coinbase.Price 45318.27
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 1
      , low   = Coinbase.Price 44281.73
      , high  = Coinbase.Price 45310.45
      , open  = Coinbase.Price 45011.16
      , close = Coinbase.Price 44955.85
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ hour * 0
      , low   = Coinbase.Price 41317.00
      , high  = Coinbase.Price 45360.45
      , open  = Coinbase.Price 45330.00
      , close = Coinbase.Price 44957.03
      }
  ]
  where
    day = Time.fromGregorian 2021 9 12

    hour = 60 * 60

sampleMinuteCandles :: [Coinbase.Candle]
sampleMinuteCandles =
  [ Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 55
      , low   = Coinbase.Price 45913.97
      , high  = Coinbase.Price 45981.38
      , open  = Coinbase.Price 45915.11
      , close = Coinbase.Price 45913.97
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 54
      , low   = Coinbase.Price 45915.09
      , high  = Coinbase.Price 46021.38
      , open  = Coinbase.Price 46021.38
      , close = Coinbase.Price 45915.09
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 53
      , low   = Coinbase.Price 45873.98
      , high  = Coinbase.Price 46041.06
      , open  = Coinbase.Price 46041.06
      , close = Coinbase.Price 45873.98
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 51
      , low   = Coinbase.Price 45938.68
      , high  = Coinbase.Price 45938.68
      , open  = Coinbase.Price 45938.68
      , close = Coinbase.Price 45938.68
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 49
      , low   = Coinbase.Price 45914.44
      , high  = Coinbase.Price 45914.46
      , open  = Coinbase.Price 45914.46
      , close = Coinbase.Price 45914.44
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 48
      , low   = Coinbase.Price 45914.44
      , high  = Coinbase.Price 45938.68
      , open  = Coinbase.Price 45938.68
      , close = Coinbase.Price 45914.46
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 47
      , low   = Coinbase.Price 45937.92
      , high  = Coinbase.Price 45937.94
      , open  = Coinbase.Price 45937.94
      , close = Coinbase.Price 45937.92
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 46
      , low   = Coinbase.Price 45930.91
      , high  = Coinbase.Price 45938.50
      , open  = Coinbase.Price 45938.50
      , close = Coinbase.Price 45937.92
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 45
      , low   = Coinbase.Price 45932.43
      , high  = Coinbase.Price 45938.50
      , open  = Coinbase.Price 45938.50
      , close = Coinbase.Price 45938.50
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 44
      , low   = Coinbase.Price 45935.39
      , high  = Coinbase.Price 45938.50
      , open  = Coinbase.Price 45935.41
      , close = Coinbase.Price 45937.95
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 43
      , low   = Coinbase.Price 45913.42
      , high  = Coinbase.Price 45935.39
      , open  = Coinbase.Price 45913.42
      , close = Coinbase.Price 45935.39
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 42
      , low   = Coinbase.Price 45912.83
      , high  = Coinbase.Price 45912.85
      , open  = Coinbase.Price 45912.85
      , close = Coinbase.Price 45912.83
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 41
      , low   = Coinbase.Price 45900.66
      , high  = Coinbase.Price 45934.66
      , open  = Coinbase.Price 45934.66
      , close = Coinbase.Price 45912.83
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 40
      , low   = Coinbase.Price 45877.31
      , high  = Coinbase.Price 45934.66
      , open  = Coinbase.Price 45879.94
      , close = Coinbase.Price 45934.66
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 39
      , low   = Coinbase.Price 45879.92
      , high  = Coinbase.Price 45934.66
      , open  = Coinbase.Price 45934.66
      , close = Coinbase.Price 45879.92
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 38
      , low   = Coinbase.Price 45891.99
      , high  = Coinbase.Price 45932.66
      , open  = Coinbase.Price 45932.66
      , close = Coinbase.Price 45891.99
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 37
      , low   = Coinbase.Price 45933.66
      , high  = Coinbase.Price 45933.68
      , open  = Coinbase.Price 45933.68
      , close = Coinbase.Price 45933.66
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 36
      , low   = Coinbase.Price 45933.66
      , high  = Coinbase.Price 45948.66
      , open  = Coinbase.Price 45948.66
      , close = Coinbase.Price 45933.66
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 35
      , low   = Coinbase.Price 45932.16
      , high  = Coinbase.Price 45948.66
      , open  = Coinbase.Price 45948.66
      , close = Coinbase.Price 45932.16
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 34
      , low   = Coinbase.Price 45920.19
      , high  = Coinbase.Price 45920.21
      , open  = Coinbase.Price 45920.19
      , close = Coinbase.Price 45920.19
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 33
      , low   = Coinbase.Price 45911.18
      , high  = Coinbase.Price 45911.20
      , open  = Coinbase.Price 45911.20
      , close = Coinbase.Price 45911.18
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 32
      , low   = Coinbase.Price 45911.18
      , high  = Coinbase.Price 45948.66
      , open  = Coinbase.Price 45948.34
      , close = Coinbase.Price 45911.18
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 31
      , low   = Coinbase.Price 45947.83
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45948.32
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 30
      , low   = Coinbase.Price 45918.46
      , high  = Coinbase.Price 45918.48
      , open  = Coinbase.Price 45918.48
      , close = Coinbase.Price 45918.46
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 29
      , low   = Coinbase.Price 45918.46
      , high  = Coinbase.Price 45918.46
      , open  = Coinbase.Price 45918.46
      , close = Coinbase.Price 45918.46
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 28
      , low   = Coinbase.Price 45921.50
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45925.09
      , close = Coinbase.Price 45921.50
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 27
      , low   = Coinbase.Price 45907.97
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45925.07
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 24
      , low   = Coinbase.Price 45965.20
      , high  = Coinbase.Price 45965.22
      , open  = Coinbase.Price 45965.22
      , close = Coinbase.Price 45965.20
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 23
      , low   = Coinbase.Price 45965.20
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45965.20
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 21
      , low   = Coinbase.Price 45956.24
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45956.24
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 20
      , low   = Coinbase.Price 45928.57
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45936.75
      , close = Coinbase.Price 45928.57
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 19
      , low   = Coinbase.Price 45930.26
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45936.73
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 18
      , low   = Coinbase.Price 45942.74
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45942.74
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 17
      , low   = Coinbase.Price 45909.05
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 46071.38
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 16
      , low   = Coinbase.Price 45901.52
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45901.52
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 11
      , low   = Coinbase.Price 45899.94
      , high  = Coinbase.Price 45899.96
      , open  = Coinbase.Price 45899.94
      , close = Coinbase.Price 45899.94
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 10
      , low   = Coinbase.Price 45899.32
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45899.32
      , close = Coinbase.Price 46071.38
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 9
      , low   = Coinbase.Price 45894.13
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45894.15
      , close = Coinbase.Price 45913.87
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 8
      , low   = Coinbase.Price 45894.13
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45894.13
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 6
      , low   = Coinbase.Price 45876.56
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 46071.38
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 5
      , low   = Coinbase.Price 45864.29
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45864.29
      , close = Coinbase.Price 45880.29
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 4
      , low   = Coinbase.Price 45864.29
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45866.88
      , close = Coinbase.Price 45864.31
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 3
      , low   = Coinbase.Price 45854.80
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 45854.80
      , close = Coinbase.Price 45866.86
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 2
      , low   = Coinbase.Price 45854.73
      , high  = Coinbase.Price 46071.38
      , open  = Coinbase.Price 46071.38
      , close = Coinbase.Price 45854.82
      }
  , Coinbase.Candle
      { time  = Time.UTCTime day $ mkTime 1
      , low   = Coinbase.Price 45857.43
      , high  = Coinbase.Price 45857.43
      , open  = Coinbase.Price 45857.43
      , close = Coinbase.Price 45857.43
      }
  ]

  where
    day = Time.fromGregorian 2021 9 12

    mkTime minute = 60 * 60 * 17 + minute * 60
