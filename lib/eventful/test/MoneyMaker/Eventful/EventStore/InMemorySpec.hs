{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -Wno-error=partial-type-signatures #-}

module MoneyMaker.Eventful.EventStore.InMemorySpec
  ( spec
  )
  where

import MoneyMaker.Eventful.EventSpec          hiding (spec)
import MoneyMaker.Eventful.EventStore.Helpers
import MoneyMaker.Eventful

import MoneyMaker.Based

import qualified Data.Aeson as Aeson
import           Test.Hspec

spec :: Spec
spec = do
  describe "MonadEventStore InMemoryEventStoreT" do
    it "executing testEventStoreProcedure results in the expected user aggregate" do
      let result :: Either (OneOf TestEventStoreProcedureErrors) _
            = fmap fst . runIdentity $ runInMemoryEventStoreT initialEventStore do
                firstAggregate <- testEventStoreProcedure

                eventStoreBefore <- dumpEventStore

                eventStoreAfter <- dumpEventStore

                void $ applyCommand userAggregateId $ SetName "Tester"

                secondAggregate <- getAggregate userAggregateId

                pure (firstAggregate, secondAggregate, eventStoreBefore, eventStoreAfter)

      case result of
        Left error ->
          expectationFailure
            $ "Test procedure should have succeeded but failed with error: "
                <> show error

        Right (firstAggregate, secondAggregate, eventStoreBefore, eventStoreAfter) -> do
          eventStoreBefore `shouldBe` eventStoreAfter

          firstAggregate  `shouldBe` exampleUser
          secondAggregate `shouldBe` exampleUser { name = Just "Tester" }

initialEventStore :: [StorableEvent]
initialEventStore =
  [ StorableEvent [uuidQuasiQuoter|123e4567-e89b-12d3-a456-426614174000|] Aeson.Null
  , StorableEvent [uuidQuasiQuoter|123e4666-e89b-12d3-a456-426614174000|] Aeson.Null
  ]
