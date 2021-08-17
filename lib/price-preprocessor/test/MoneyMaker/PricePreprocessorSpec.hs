module MoneyMaker.PricePreprocessorSpec
  ( spec
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful
import MoneyMaker.PricePreprocessor

import Protolude
import Test.Hspec

import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock    as Time

spec :: Spec
spec = do
  describe "Eventful SwingEvent" $ do
    it "extracts all swings from price data" $ do
      let result :: Either (OneOf '[Void, CouldntDecodeEventError, NoEventsFoundError]) Swings
            = fmap fst $ runIdentity $ runInMemoryEventStoreT [] $ do
                let id = Id [uuid|123e4567-e89b-12d3-a456-426614174000|]

                _ <- forM prices $ applyCommand id . uncurry AddNewPrice

                getAggregate @SwingEvent id

      result `shouldBe` Right expectedAggregate

expectedAggregate :: Swings
expectedAggregate
  = SwingUp
  $ High (Price 9) (mkTime 12) . Just
  $ Low (Price 4) (mkTime 9) . Just
  $ High (Price 12) (mkTime 6) . Just
  $ Low (Price 1) (mkTime 1) Nothing

prices :: [(Price, Time.UTCTime)]
prices = zip priceValues (mkTime <$> [1..fromIntegral (length priceValues)])
  where
    priceValues
      = Price <$> [1, 3, 2, 10, 7, 12, 8, 11, 4, 6, 5, 9]

mkTime :: Integer -> Time.UTCTime
mkTime num
  = Time.UTCTime
      (Time.fromGregorian 2021 07 31)
      (Time.secondsToDiffTime (19 * 60 * 60 + 37 * 60 + num))
