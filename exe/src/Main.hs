{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE QuantifiedConstraints #-}

module Main where

import Environment

import MoneyMaker.Based

import qualified MoneyMaker.Coinbase.SDK      as Coinbase
import qualified MoneyMaker.Eventful          as Eventful
import qualified MoneyMaker.PricePreprocessor as Preprocessor

import qualified Control.Concurrent.STM      as STM
import qualified Control.Monad.Logger        as Logger
import qualified Data.Pool                   as Pool
import qualified Data.Time                   as Time
import qualified Database.Persist.Postgresql as Postgres
import qualified Database.Persist.Sql        as Persist
import qualified Prelude
import qualified System.IO                   as IO
import qualified Wuss

deriving newtype instance Eventful.MonadEventStore m
  => Eventful.MonadEventStore (Coinbase.SandboxCoinbaseRestT m)

main :: IO ()
main = do
  runUltraExceptTWithoutErrors $ Coinbase.runSandboxCoinbaseRestT $ handleAllErrors
    @'[Coinbase.ServantClientError, Coinbase.HeaderError]
    ( print =<< Coinbase.getCandles
        (Coinbase.TradingPair Coinbase.BTC Coinbase.USD)
        (Time.UTCTime (Time.fromGregorian 2021 9 12) (17 * 60 * 60))
        (Time.UTCTime (Time.fromGregorian 2021 9 12) (17 * 60 * 60 + 55 * 60))
        Coinbase.OneMinute
    )
    print
    print

  when False $ do
    Environment{..} <- getEnvironment

    when (mode == ProdMode)
      $ Prelude.error "Prod is NOT READY!"

    IO.hSetBuffering IO.stdin IO.NoBuffering -- we only need this for testing with getLine
    IO.hSetBuffering IO.stdout IO.NoBuffering -- we only need this for testing with getLine

    (priceDataQueue, predictionQueue) <- spawnPredictionProcessAndBindToQueues

    let connectionString
          =  "host=localhost user=" <> user
          <> " dbname=" <> dbName
          <> " password=" <> password

    Logger.runNoLoggingT
      $ Postgres.withPostgresqlPool connectionString 2
      $ \connectionPool -> liftIO $ do
          Pool.withResource connectionPool
            $ runReaderT $ Persist.runMigration Eventful.migrateAll

          void $ forkIO $ getLivePriceData connectionPool mode priceDataQueue

          void $ forever $ do
            executionOrders <- STM.atomically $ STM.readTQueue predictionQueue
            executeNewOrders executionOrders

  where
    -- placeholder for the function that will take a new prediction from the
    -- prediction process and evaluate whether it needs to make any changes
    -- to the portfolio based on that
    executeNewOrders Preprocessor.ExecutionOrders{..} = do
      putStrLn message


getLivePriceData
  :: Postgres.ConnectionPool
  -> Mode
  -> STM.TQueue Preprocessor.PriceData
  -> IO ()
getLivePriceData connectionPool mode priceDataQueue = do
  let websocketHost = case mode of
        ProdMode -> "ws-feed.pro.coinbase.com"
        TestMode -> "ws-feed-public.sandbox.pro.coinbase.com"

  Wuss.runSecureClient websocketHost 443 "/"
    $ Coinbase.websocketsClient \newPriceData ->
        Eventful.runSqlEventStoreTWithoutErrors connectionPool
          $ Coinbase.runSandboxCoinbaseRestT
          $ handleAllErrors @ProcessPriceDataErrors
              (processPriceData priceDataQueue newPriceData)

              (\Eventful.NoEventsFoundError -> putStrLn @Text "No events found")

              (\(Eventful.CouldntDecodeEventError err) ->
                putStrLn $ "Couldn't decode event error: " <> err)

              (print @_ @Preprocessor.NoNewCandlesFoundError)

              (print @_ @Coinbase.ServantClientError)

              (print @_ @Coinbase.HeaderError)

              (print @_ @Preprocessor.TimeOfPreviousSaveIsLaterThanCurrentTimeError)

type ProcessPriceDataErrors =
  '[ Eventful.NoEventsFoundError
   , Eventful.CouldntDecodeEventError
   , Preprocessor.NoNewCandlesFoundError
   , Coinbase.ServantClientError
   , Coinbase.HeaderError
   , Preprocessor.TimeOfPreviousSaveIsLaterThanCurrentTimeError
   ]

processPriceData
  :: ( forall errors. MonadIO (m errors)
     , Eventful.MonadEventStore m
     , Coinbase.CoinbaseRestAPI m
     , MonadPrinter m
     )
  => STM.TQueue Preprocessor.PriceData
  -> Coinbase.TickerPriceData
  -> m ProcessPriceDataErrors ()

processPriceData priceDataQueue newPriceData = do
  priceData <-
    Preprocessor.preprocessPriceData @ProcessPriceDataErrors newPriceData

  liftIO $ STM.atomically $ STM.writeTQueue priceDataQueue priceData


spawnPredictionProcessAndBindToQueues
  :: IO
       ( STM.TQueue Preprocessor.PriceData
       , STM.TQueue Preprocessor.ExecutionOrders
       )
spawnPredictionProcessAndBindToQueues = do
  priceDataQueue <- STM.newTQueueIO
  predictionsQueue <- STM.newTQueueIO

  -- need a separate thread to run the infinite loop
  void $ forkIO
    $ bindMarketReaderToFundsManager priceDataQueue predictionsQueue

  pure (priceDataQueue, predictionsQueue)


bindMarketReaderToFundsManager
  :: STM.TQueue Preprocessor.PriceData
  -> STM.TQueue Preprocessor.ExecutionOrders
  -> IO ()
bindMarketReaderToFundsManager marketQueue managerQueue = do
  _contractualPriceData <- STM.atomically $ STM.readTQueue marketQueue

  STM.atomically $ STM.writeTQueue managerQueue Preprocessor.ExecutionOrders
    { message = "Hello"
    }

  bindMarketReaderToFundsManager marketQueue managerQueue
