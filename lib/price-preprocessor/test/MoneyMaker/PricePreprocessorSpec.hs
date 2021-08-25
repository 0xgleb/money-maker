module MoneyMaker.PricePreprocessorSpec
  ( spec
  )
  where

import qualified MoneyMaker.Coinbase.SDK      as Coinbase
import qualified MoneyMaker.Error             as Error
import qualified MoneyMaker.Eventful          as Eventful
import           MoneyMaker.PricePreprocessor

import Protolude
import Test.Hspec

import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock    as Time

type Errors =
  '[ Void
   , Eventful.CouldntDecodeEventError
   , Eventful.NoEventsFoundError
   ]

spec :: Spec
spec = do
  describe "Eventful SwingEvent" $ do
    it "extracts all swings from price data" $ do
      let result :: Either (Error.OneOf Errors) Swings
            = fmap fst $ runIdentity $ Eventful.runInMemoryEventStoreT [] $ do
                let id = Eventful.Id [Eventful.uuid|123e4567-e89b-12d3-a456-426614174000|]

                _ <- forM prices $ Eventful.applyCommand id . uncurry AddNewPrice

                Eventful.getAggregate @SwingEvent id

      -- I've drawn a chart for this example so that it's easier to understand what's going on
      -- You can find it in docs/swings-example.png
      result `shouldBe` Right expectedAggregate

expectedAggregate :: Swings
expectedAggregate
  = SwingUp
  $ High (Coinbase.Price 9) (mkTime 12) . Just
  $ Low (Coinbase.Price 4) (mkTime 9) . Just
  $ High (Coinbase.Price 12) (mkTime 6) . Just
  $ Low (Coinbase.Price 1) (mkTime 1) Nothing

prices :: [(Coinbase.Price, Time.UTCTime)]
prices = zip priceValues (mkTime <$> [1..fromIntegral (length priceValues)])
  where
    priceValues
      = Coinbase.Price <$> [1, 3, 2, 10, 7, 12, 8, 11, 4, 6, 5, 9]

mkTime :: Integer -> Time.UTCTime
mkTime num
  = Time.UTCTime
      (Time.fromGregorian 2021 07 31)
      (Time.secondsToDiffTime (19 * 60 * 60 + 37 * 60 + num))