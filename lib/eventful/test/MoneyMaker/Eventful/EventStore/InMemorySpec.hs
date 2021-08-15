{-# LANGUAGE QuasiQuotes #-}

module MoneyMaker.Eventful.EventStore.InMemorySpec
  ( spec
  )
  where

import MoneyMaker.Eventful.EventSpec          hiding (spec)
import MoneyMaker.Eventful.EventStore.Helpers

import MoneyMaker.Error
import MoneyMaker.Eventful

import Protolude

import qualified Data.Aeson as Aeson
import           Test.Hspec

spec :: Spec
spec = do
  describe "applyCommand and getAggregate" $ do
    it "executing testEventStoreProcedure results in the expected user aggregate" $ do
      let result :: Either (OneOf TestEventStoreProcedureErrors) User
            = fmap fst . runIdentity . runUltraExceptT
            . flip runStateT exampleStorableEvents . runInMemoryEventStore
            $ testEventStoreProcedure
      result `shouldBe` (Right exampleUser)

exampleStorableEvents :: [StorableEvent]
exampleStorableEvents
  = [ StorableEvent [uuid|123e4567-e89b-12d3-a456-426614174000|] Aeson.Null ]
  -- <> storableUserEvents
  <> [ StorableEvent [uuid|123e4666-e89b-12d3-a456-426614174000|] Aeson.Null ]
  -- where
  --   storableUserEvents = exampleUserEvents <&> \event ->
  --     StorableEvent [uuid|123e4666-e89b-12d3-a456-666614174000|] $ Aeson.toJSON event
