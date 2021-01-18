{-# LANGUAGE TemplateHaskell #-}

module MoneyMaker.EventStoreSpec (spec) where

import MoneyMaker.EventSpec hiding (spec)
import MoneyMaker.EventStore
import MoneyMaker.Event
import MoneyMaker.Error
import MoneyMaker.UUID

import qualified Data.Aeson as Aeson
import           Protolude
import           Test.Hspec

spec :: Spec
spec = do
  describe "getAggregate" $ do
    it "correctly picks events and computes the aggregate" $ do
      let result
            = fmap fst $ getUltraEither
            $ flip runStateT exampleStorableEvents $ runInMemoryEventStore
            $ getAggregate @UserEvent @[NoEventsFoundError, UserEventError, CouldntDecodeEventError]
            $ Id [uuid|123e4666-e89b-12d3-a456-666614174000|]
      result `shouldBe` (Right exampleUser)

exampleStorableEvents :: [StorableEvent]
exampleStorableEvents
  = [ StorableEvent [uuid|123e4567-e89b-12d3-a456-426614174000|] Aeson.Null ]
  <> storableUserEvents
  <> [ StorableEvent [uuid|123e4666-e89b-12d3-a456-426614174000|] Aeson.Null ]
  where
    storableUserEvents = exampleUserEvents <&> \event ->
      StorableEvent [uuid|123e4666-e89b-12d3-a456-666614174000|] $ Aeson.toJSON event
