module MoneyMaker.PricePreprocessorSpec
  ( spec
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful
import MoneyMaker.PricePreprocessor

import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "Eventful SwingEvent" $ do
    it "extracts all swings from price data" $ do
      let result :: Either (OneOf '[CouldntDecodeEventError, NoEventsFoundError]) Swings
            = fmap fst $ runIdentity $ runInMemoryEventStoreT [] $ do
                let id = Id [uuid|123e4567-e89b-12d3-a456-426614174000|]

                _ <- forM prices $ applyCommand id . AddNewPrice

                getAggregate @SwingEvent id

      result `shouldBe` (Right expectedAggregate)

expectedAggregate :: Swings
expectedAggregate
  = SwingUp
  $ High (Price 9) . Just
  $ Low (Price 4) . Just
  $ High (Price 12) . Just
  $ Low (Price 1) Nothing

prices :: [Price]
prices = Price <$> [1, 3, 2, 10, 7, 12, 8, 11, 4, 6, 5, 9]
