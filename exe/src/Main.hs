module Main where

import Protolude

import qualified Control.Concurrent.STM as STM
import qualified Data.Text.Lazy.IO      as Txt.LIO
import qualified Paths_exe              as Path
import qualified System.IO              as IO
import qualified System.Process         as Proc

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering -- we only need this for testing with getLine
  IO.hSetBuffering IO.stdout IO.NoBuffering -- we only need this for testing with getLine

  (priceDataChannel, predictionChannel) <- spawnChannelledPredictionProcess

  void $ forkIO $ forever $ getLivePriceData priceDataChannel

  void $ forever $ do
    prediction <- STM.atomically $ STM.readTChan predictionChannel
    handlePrediction prediction

  where
    -- placeholder for the function that will get live price data from the
    -- Binance Websockets API, process it, and write relevant information
    -- into the price data channel
    getLivePriceData :: STM.TChan ContractualPriceData -> IO ()
    getLivePriceData priceDataChannel = do
      message <- getLine
      STM.atomically $ STM.writeTChan priceDataChannel ContractualPriceData{..}

    -- placeholder for the function that will take a new prediction from the
    -- prediction process and evaluate whether it needs to make any changes
    -- to the portfolio based on that
    handlePrediction ContractualPrediction{..} = do
      putStrLn $ "Got back: " <> message

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData
  = ContractualPriceData
      { message :: Text
      }

data ContractualPrediction
  = ContractualPrediction
      { message :: LText
      }

spawnChannelledPredictionProcess
  :: IO (STM.TChan ContractualPriceData, STM.TChan ContractualPrediction)
spawnChannelledPredictionProcess = do
  priceDataChannel <- STM.newTChanIO
  predictionsChannel <- STM.newTChanIO

  (inputHandle, outputHandle, processHandle) <- createProcess "example"
  -- TODO: try pinging the process before spawning everything else

  -- need a separate thread to run the infinite loop
  void $ forkIO $ do
    infinitePriceChannelToInputLink priceDataChannel inputHandle
    void $ Proc.terminateProcess processHandle

  -- need a separate thread to run the infinite loop
  void $ forkIO
    $ infiniteOutputToPredictionsChannelLink outputHandle predictionsChannel

  pure (priceDataChannel, predictionsChannel)

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

-- | This function reads from the channel whenever there is something in there
-- and writes the new data into the input handle of the predictions process.
-- The reason it only reads from the channel whenver there is something in there
-- and doesn't just continuously run in an infinite loop is the @atomically@
-- function, which blocks execution until there is something in the channel
infinitePriceChannelToInputLink
  :: STM.TChan ContractualPriceData
  -> InputHandle
  -> IO ()
infinitePriceChannelToInputLink priceDataChannel inputHandle = do
  ContractualPriceData{..} <- STM.atomically $ STM.readTChan priceDataChannel
  let shouldStop = message == "stop"
  unless shouldStop $ do
    hPutStrLn @Text (unpackInputHandle inputHandle) message
    infinitePriceChannelToInputLink priceDataChannel inputHandle

newtype OutputHandle
  = OutputHandle { unpackOutputHandle :: Handle }
  deriving newtype (Show)

-- | This function reads from the prediction process output and writes new
-- predictions into the prediction channel whenever there is a new line with a
-- prediction in the output
infiniteOutputToPredictionsChannelLink
  :: OutputHandle
  -> STM.TChan ContractualPrediction
  -> IO ()
infiniteOutputToPredictionsChannelLink
  outputHandle@(OutputHandle unpackedOutputHandle)
  predictionsChannel
  = do
  closed <- IO.hIsClosed unpackedOutputHandle
  unless closed $ do
    message <- Txt.LIO.hGetLine unpackedOutputHandle
    STM.atomically $ STM.writeTChan predictionsChannel ContractualPrediction{..}
    infiniteOutputToPredictionsChannelLink outputHandle predictionsChannel
