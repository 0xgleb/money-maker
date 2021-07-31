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
  describe "FromJSON CoinbaseMessage" $ do
    it "can decode ticker data" $ do
      Aeson.fromJSON sampleTickerMessageJSON `shouldBe` Aeson.Success sampleTickerMessage

  describe "ToJSON SubscribeMessage" $ do
    it "correctly encodes into the sample payload" $ do
      Aeson.toJSON sampleSubscribeMessage `shouldBeUnorderedJson` sampleSubscribeMessageJSON

sampleTickerMessage :: CoinbaseMessage
sampleTickerMessage
  = Ticker ContractualPriceData
      { productId = TradingPair BTC USD
      , price = "41438.8"
      }

sampleTickerMessageJSON :: Aeson.Value
sampleTickerMessageJSON =
  [Aeson.aesonQQ|
  {
      "last_size": "0.0065",
      "trade_id": 32205173,
      "time": "2021-07-31T19:37:38.975749Z",
      "side": "sell",
      "best_ask": "41478.80",
      "best_bid": "41419.04",
      "volume_30d": "497645393.29741923",
      "high_24h": "42478.8",
      "low_24h": "39786.02",
      "volume_24h": "11530.86090963",
      "open_24h": "39794.24",
      "price": "41438.8",
      "product_id": "BTC-USD",
      "sequence": 363177342,
      "type": "ticker"
  }
  |]


sampleSubscribeMessage :: SubscribeMessage
sampleSubscribeMessage
  = SubscribeMessage
      { productIds =
          [ TradingPair ETH USD
          , TradingPair ETH EUR
          ]
      , channels =
          [ Level2Channel []
          , HeartbeatChannel []
          , TickerChannel
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
