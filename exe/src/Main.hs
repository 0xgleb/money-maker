module Main where

import Contract
import Environment

import qualified MoneyMaker.Coinbase.SDK.Websockets as Coinbase
import qualified MoneyMaker.Eventful                as Eventful

import Protolude

import qualified Control.Concurrent.STM      as STM
import qualified Control.Monad.Logger        as Logger
import qualified Data.Aeson                  as Aeson
import qualified Data.Pool                   as Pool
import qualified Data.Text.Lazy.IO           as Txt.LIO
import qualified Database.Persist.Postgresql as Postgres
import qualified Database.Persist.Sql        as Persist
import qualified Paths_exe                   as Path
import qualified Prelude
import qualified System.IO                   as IO
import qualified System.Process              as Proc
import qualified Wuss

main :: IO ()
main = do
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
          prediction <- STM.atomically $ STM.readTQueue predictionQueue
          handlePrediction prediction

  where
    -- placeholder for the function that will get live price data from the
    -- Coinbase Pro Websockets API, process it, and write relevant information
    -- into the price data queue
    getLivePriceData
      :: Postgres.ConnectionPool
      -> Mode
      -> STM.TQueue ContractualPriceData
      -> IO ()
    getLivePriceData connectionPool mode priceDataQueue = do
      let websocketHost = case mode of
            ProdMode -> "ws-feed.pro.coinbase.com"
            TestMode -> "ws-feed-public.sandbox.pro.coinbase.com"

      Wuss.runSecureClient websocketHost 443 "/"
        $ Coinbase.websocketsClient $ \newPriceData -> do
            priceDataOrErrors <-
              Eventful.runSqlEventStoreT @'[] connectionPool
                $ toContractualPriceData newPriceData

            case priceDataOrErrors of
              Right priceData ->
                STM.atomically $ STM.writeTQueue priceDataQueue priceData
              Left error ->
                case error of

    -- placeholder for the function that will take a new prediction from the
    -- prediction process and evaluate whether it needs to make any changes
    -- to the portfolio based on that
    handlePrediction ContractualPrediction{..} = do
      putStrLn $ "Got back: \"" <> message <> "\""

spawnPredictionProcessAndBindToQueues
  :: IO (STM.TQueue ContractualPriceData, STM.TQueue ContractualPrediction)
spawnPredictionProcessAndBindToQueues = do
  priceDataQueue <- STM.newTQueueIO
  predictionsQueue <- STM.newTQueueIO

  (inputHandle, outputHandle, processHandle) <- createProcess "example"
  -- TODO: try pinging the process before spawning everything else

  -- need a separate thread to run the infinite loop
  void $ forkIO $ do
    bindPriceDataQueueToProcessInput priceDataQueue inputHandle
    void $ Proc.terminateProcess processHandle

  -- need a separate thread to run the infinite loop
  void $ forkIO
    $ bindProcessOutputToPredictionsQueue outputHandle predictionsQueue

  pure (priceDataQueue, predictionsQueue)

  where
    createProcess strategy = do
      scriptPath <- Path.getDataFileName $ "../soothsayer/" <> strategy <> ".py"

      (Just inputHandle, Just outputHandle, _, processHandle) <-
        Proc.createProcess (Proc.proc "/usr/bin/python3" [scriptPath])
          { Proc.std_in = Proc.CreatePipe, Proc.std_out = Proc.CreatePipe }

      IO.hSetBuffering inputHandle  IO.NoBuffering
      IO.hSetBuffering outputHandle IO.NoBuffering

      pure (InputHandle inputHandle, OutputHandle outputHandle, processHandle)

newtype InputHandle
  = InputHandle { unpackInputHandle :: Handle }
  deriving newtype (Show)

-- | This function reads from the queue whenever there is something in there
-- and writes the new data into the input handle of the predictions process.
-- The reason it only reads from the queue whenver there is something in there
-- and doesn't just continuously run in an infinite loop is the @atomically@
-- function, which blocks execution until there is something in the queue
bindPriceDataQueueToProcessInput
  :: STM.TQueue ContractualPriceData
  -> InputHandle
  -> IO ()
bindPriceDataQueueToProcessInput priceDataQueue inputHandle = do
  contractualPriceData <- STM.atomically $ STM.readTQueue priceDataQueue

  hPutStrLn (unpackInputHandle inputHandle) $ Aeson.encode contractualPriceData

  bindPriceDataQueueToProcessInput priceDataQueue inputHandle

newtype OutputHandle
  = OutputHandle { unpackOutputHandle :: Handle }
  deriving newtype (Show)

-- | This function reads from the prediction process output and writes new
-- predictions into the prediction queue whenever there is a new line with a
-- prediction in the output
bindProcessOutputToPredictionsQueue
  :: OutputHandle
  -> STM.TQueue ContractualPrediction
  -> IO ()
bindProcessOutputToPredictionsQueue
  outputHandle@(OutputHandle unpackedOutputHandle)
  predictionsQueue
  = do
  closed <- IO.hIsClosed unpackedOutputHandle
  unless closed $ do
    message <- Txt.LIO.hGetLine unpackedOutputHandle
    STM.atomically $ STM.writeTQueue predictionsQueue ContractualPrediction{..}
    bindProcessOutputToPredictionsQueue outputHandle predictionsQueue
