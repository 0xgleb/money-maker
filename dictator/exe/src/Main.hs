module Main where

import Protolude

import qualified System.Process as Proc
import qualified Control.Concurrent.STM as STM
import qualified System.IO as IO

main :: IO ()
main = do
  (priceDataChannel, predictionChannel, processHandle) <-
    spawnChannelledPredictionProcess

  void $ forkIO $ getLivePriceData priceDataChannel

  void $ forever $ do
    prediction <- STM.atomically $ STM.readTChan predictionChannel
    handlePrediction prediction

  void $ Proc.waitForProcess processHandle
  where
    -- placeholder for the function that will get live price data from the
    -- Binance Websockets API, process it, and write relevant information
    -- into the price data channel
    getLivePriceData _priceDataChannel = undefined

    -- placeholder for the function that will take a new prediction from the
    -- prediction process and evaluate whether it needs to make any changes
    -- to the portfolio based on that
    handlePrediction _prediction = undefined

-- I think using "Contractual" prefix can help identify which types have to have
-- a certain encoding to not break the contract with the prediction mechanism
data ContractualPriceData = ContractualPriceData {}
data ContractualPrediction = ContractualPrediction {}

spawnChannelledPredictionProcess
  :: IO (STM.TChan ContractualPriceData, STM.TChan ContractualPrediction, Proc.ProcessHandle)
spawnChannelledPredictionProcess = do
  priceDataChannel <- STM.newTChanIO
  predictionsChannel <- STM.newTChanIO

  (fmap InputHandle -> Just inputHandle
    , fmap OutputHandle -> Just outputHandle
    , _
    , processHandle
    ) <- Proc.createProcess processDescription

  -- hSetBuffering inputHandle NoBuffering
  -- hSetBuffering outputHandle NoBuffering

  -- need a separate thread to run the infinite loop
  void $ forkIO $ infinitePriceChannelToInputLink priceDataChannel inputHandle

  -- need a separate thread to run the infinite loop
  void $ forkIO $ infiniteOutputToPredictionsChannelLink outputHandle predictionsChannel

  pure (priceDataChannel, predictionsChannel, processHandle)

  where
    pythonFile = undefined
    processDescription
      = (Proc.proc ("python " <> pythonFile) [])
          { Proc.std_in = Proc.CreatePipe, Proc.std_out = Proc.CreatePipe }

newtype InputHandle
  = InputHandle { unpackInputHandle :: Handle }

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
  _priceData <- STM.atomically $ STM.readTChan priceDataChannel
  let shouldStop = False -- just a placeholder
  unless shouldStop $ do
    hPutStrLn @ByteString (unpackInputHandle inputHandle) "just a test"
    infinitePriceChannelToInputLink priceDataChannel inputHandle

newtype OutputHandle
  = OutputHandle { unpackOutputHandle :: Handle }

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
    _prediction <- IO.hGetLine unpackedOutputHandle -- maybe `catch` exceptions???
    STM.atomically $ STM.writeTChan predictionsChannel ContractualPrediction{}
    infiniteOutputToPredictionsChannelLink outputHandle predictionsChannel
