module MoneyMaker.Coinbase.SDK.ContractSpec
  ( spec
  )
  where

import MoneyMaker.Coinbase.SDK.Contract

import Protolude

import qualified Data.Aeson    as Aeson
import qualified Data.Aeson.QQ as Aeson
import qualified Timestamp

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "FromJSON Candle" $ do
    it "can decode candle data" $ do
      Aeson.fromJSON sampleCandlesJSON `shouldBe` Aeson.Success sampleCandles

  describe "TradingPair JSON encodings" $ do
    it "can decode BTC-USD" $ do
      Aeson.decode "\"BTC-USD\"" `shouldBe` Just (TradingPair BTC USD)

    it "holds property decode . encode === Just . identity"
      $ property $ \pair ->
        Aeson.decode (Aeson.encode @TradingPair pair) == Just pair

sampleCandles :: [Candle]
sampleCandles =
  [ Candle
      { time  = Timestamp.timestampUtcTime $ Timestamp.Timestamp 1630357200
      , low   = Price 48206.23
      , high  = Price 48737.6
      , open  = Price 48669.93
      , close = Price 48413.23
      }
  , Candle
      { time  = Timestamp.timestampUtcTime $ Timestamp.Timestamp 1630353600
      , low   = Price 48559.71
      , high  = Price 48737.6
      , open  = Price 48644.01
      , close = Price 48669.91
      }
  , Candle
      { time  = Timestamp.timestampUtcTime $ Timestamp.Timestamp 1630350000
      , low   = Price 48393.99
      , high  = Price 48644.01
      , open  = Price 48520.02
      , close = Price 48643.99
      }
  ]

sampleCandlesJSON :: Aeson.Value
sampleCandlesJSON =
  [Aeson.aesonQQ|
  [ [1630357200, 48206.23, 48737.6,  48669.93, 48413.23, 20.08915164]
  , [1630353600, 48559.71, 48737.6,  48644.01, 48669.91, 24.37300803]
  , [1630350000, 48393.99, 48644.01, 48520.02, 48643.99, 19.99672024]
  ]
  |]
