module MoneyMaker.Coinbase.SDK.WebsocketsSpec
  ( spec
  )
  where

import MoneyMaker.Coinbase.SDK.Websockets

import           Protolude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ as Aeson

import Test.Hspec
import Test.Hspec.Expectations.Json

spec :: Spec
spec = do
  describe "ToJSON SubscribeMessage" $ do
    it "correctly encodes into the sample payload" $ do
      Aeson.toJSON sampleSubscribeMessage `shouldBeUnorderedJson` sampleSubscribeMessageJSON

sampleSubscribeMessage :: SubscribeMessage
sampleSubscribeMessage
  = SubscribeMessage
      { productIds =
          [ TradingPair ETH USD
          , TradingPair ETH EUR
          ]
      , channels =
          [ Level2
          , Heartbeat
          , Ticker
              [ TradingPair ETH BTC
              , TradingPair ETH USD
              ]
          ]
      }

sampleSubscribeMessageJSON :: Aeson.Value
sampleSubscribeMessageJSON =
  [Aeson.aesonQQ|
  {
      "type": "subscribe",
      "product_ids": [
          "ETH-USD",
          "ETH-EUR"
      ],
      "channels": [
          "level2",
          "heartbeat",
          {
              "name": "ticker",
              "product_ids": [
                  "ETH-BTC",
                  "ETH-USD"
              ]
          }
      ]
  }
  |]
