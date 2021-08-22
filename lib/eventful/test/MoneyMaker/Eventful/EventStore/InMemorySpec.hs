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
  describe "MonadEventStore InMemoryEventStoreT" $ do
    it "executing testEventStoreProcedure results in the expected user aggregate" $ do
      let result :: Either (OneOf TestEventStoreProcedureErrors) User
            = fmap fst . runIdentity
            $ runInMemoryEventStoreT initialEventStore testEventStoreProcedure

      result `shouldBe` Right exampleUser

initialEventStore :: [StorableEvent]
initialEventStore
  =  [ StorableEvent [uuid|123e4567-e89b-12d3-a456-426614174000|] Aeson.Null ]
  <> [ StorableEvent [uuid|123e4666-e89b-12d3-a456-426614174000|] Aeson.Null ]
