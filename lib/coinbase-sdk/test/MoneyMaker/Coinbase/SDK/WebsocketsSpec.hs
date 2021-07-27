module MoneyMaker.Coinbase.SDK.WebsocketsSpec
  ( spec
  )
  where

import MoneyMaker.Coinbase.SDK.Websockets

import           Protolude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ as Aeson

import Test.Hspec

spec :: Spec
spec = _ -- do
  describe "ToJSON SubscribeMessage" $ do
    it "correctly encodes into the sample payload" $ do
  --     getUltraEither (computeCurrentState @_ @[NoEventsFoundError, UserEventError] exampleUserEvents)
  --       `shouldBe` (Right exampleUser)

samplePayload :: Aeson.Value
samplePayload =
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
