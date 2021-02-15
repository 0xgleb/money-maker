module MoneyMaker.Binance.Websockets
  (
  )
  where

import qualified Control.Concurrent.STM as STM
import qualified Network.Socket         as Socket
import qualified Network.WebSockets     as WS
import           Protolude

app :: STM.TQueue a -> WS.ClientApp ()
app _queue conn = do
  putStrLn @Text "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $ forever $ do
      msg <- WS.receiveData conn
      liftIO $ putStrLn msg

  -- Read from stdin and write to WS
  let loop = do
        line <- getLine
        unless (line == "") $ WS.sendTextData conn line >> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)
