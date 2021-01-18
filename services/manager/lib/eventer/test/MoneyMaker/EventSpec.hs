{-# LANGUAGE DeriveAnyClass #-}

module MoneyMaker.EventSpec
  ( spec
  , UserEvent(..)
  , Role(..)
  , User(..)
  , UserEventError(..)
  , exampleUserEvents
  , exampleUser
  )
  where

import MoneyMaker.Event
import MoneyMaker.Error

import Protolude
import Test.Hspec
import qualified Data.Aeson as Aeson

spec :: Spec
spec = do
  describe "computeCurrentState" $ do
    it "correctly computes example user events" $ do
      getUltraEither (computeCurrentState @_ @[NoEventsFoundError, UserEventError] exampleUserEvents)
        `shouldBe` (Right exampleUser)

data UserEvent
  = UserCreated
  | NameUpdated Text
  | RoleSet Role
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Role
  = Engineer
  | Manager
  | Genius
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data User
  = User
      { name :: Maybe Text
      , role :: Maybe Role
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data UserEventError
  = CantUpdateNonExistentUserError
  | CantCreateExistingUser
  deriving stock (Eq, Show)

instance Event UserEvent where
  type AggregateIdTag UserEvent = "User"
  type EventAggregate UserEvent = User
  type EventError     UserEvent = UserEventError

  applyEvent Nothing = \case
    UserCreated ->
      pure $ User Nothing Nothing

    NameUpdated _ ->
      throwUltraError CantUpdateNonExistentUserError

    RoleSet _ ->
      throwUltraError CantUpdateNonExistentUserError

  applyEvent (Just user) = \case
    UserCreated ->
      throwUltraError CantCreateExistingUser

    NameUpdated newName ->
      pure $ user { name = Just newName }

    RoleSet newRole ->
      pure $ user { role = Just newRole }

exampleUserEvents :: [UserEvent]
exampleUserEvents =
  [ UserCreated
  , NameUpdated "Creator"
  , RoleSet Genius
  , NameUpdated "Gleb"
  , RoleSet Engineer
  ]

exampleUser :: User
exampleUser = User (Just "Gleb") (Just Engineer)
