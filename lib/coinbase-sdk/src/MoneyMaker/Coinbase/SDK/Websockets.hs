{-# LANGUAGE DeriveAnyClass #-}

module MoneyMaker.Coinbase.SDK.Websockets
  ( app

  , SubscribeMessage(..)
  , TradingPair(..)
  , Currency(..)
  , Channel(..)
  )
  where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
-- import qualified Network.Socket         as Socket
import qualified Network.WebSockets     as WS
import           Protolude

app :: STM.TQueue a -> WS.ClientApp ()
app _queue conn = do
  putStrLn @Text "Connected!"

  -- TODO: send a subscribe message

  -- Fork a thread that writes WS data to stdout
  -- _ <- forkIO $ forever $ do
  _ <- forever $ do
      msg <- WS.receiveData conn
      liftIO $ putStrLn @Text msg

  -- -- Read from stdin and write to WS
  -- let loop = do
  --       line <- getLine
  --       unless (line == "") $ WS.sendTextData conn line >> loop

  -- loop

  WS.sendClose conn ("Bye!" :: Text)

data SubscribeMessage
  = SubscribeMessage
      { productIds :: [TradingPair]
      , channels   :: [Channel]
      }

instance Aeson.ToJSON SubscribeMessage where
  toJSON SubscribeMessage{..} = Aeson.object
    [ "product_ids" .= productIds
    , "channels" .= channels
    , "type" .= ("subscribe" :: Text)
    ]

data TradingPair
  = TradingPair
      { baseCurrency :: Currency
      , quoteCurrency :: Currency
      }

instance Aeson.ToJSON TradingPair where
  toJSON TradingPair{..}
    = Aeson.String $ show baseCurrency <> "-" <> show quoteCurrency

data Currency
  = ETH
  | EUR
  | USD
  | BTC
  deriving stock (Show)

data Channel
  = Level2
  | Heartbeat
  | Ticker [TradingPair]

instance Aeson.ToJSON Channel where
  toJSON = \case
    Level2 ->
      Aeson.String "level2"
    Heartbeat ->
      Aeson.String "heartbeat"
    Ticker pairs ->
      Aeson.object
        [ "name" .= ("ticker" :: Text)
        , "product_ids" .= pairs
        ]
