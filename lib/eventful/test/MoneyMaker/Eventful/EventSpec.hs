{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module MoneyMaker.Eventful.EventSpec
  ( spec

  , Role(..)
  , User(..)
  , UserEvent(..)
  , UserEventError(..)
  , exampleUserEvents
  , exampleUser

  , UserCommand(..)
  , UserCommandError(..)
  , UserAlreadyExistsError(..)
  , UserDoesntExistError(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Event
import MoneyMaker.Eventful.Command

import qualified Data.Aeson as Aeson
import           Protolude
import           Test.Hspec

spec :: Spec
spec = do
  describe "computeCurrentState" $ do
    it "correctly computes example user events" $ do
      let currentState
            = computeCurrentState @_ @[NoEventsFoundError, UserEventError] exampleUserEvents

      getUltraEither currentState `shouldBe` (Right exampleUser)

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

instance Eventful UserEvent where
  type EventName      UserEvent = "user"
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

data UserCommand
  = CreateUser
  | SetName Text
  | SetRole Role

data UserCommandError
  = UserAlreadyExists UserAlreadyExistsError
  | UserDoesntExist UserDoesntExistError
  deriving stock (Show)

data UserAlreadyExistsError
  = UserAlreadyExistsError
      { message :: !Text
      , userId  :: !(Id "user")
      }
  deriving stock (Show)

data UserDoesntExistError
  = UserDoesntExistError
      { message :: !Text
      , userId  :: !(Id "user")
      }
  deriving stock (Show)

instance Command UserCommand UserEvent where
  type CommandError UserCommand = UserCommandError

  handleCommand
    :: Id "user"
    -> Maybe User
    -> UserCommand
    -> Either UserCommandError (NonEmpty UserEvent)

  handleCommand userId maybeUser userCommand
    = case (maybeUser, userCommand) of
        (Just _, CreateUser) ->
          Left $ UserAlreadyExists UserAlreadyExistsError
            { message = "Received CreateUser command but this user already exists"
            , userId
            }
        (Nothing, CreateUser) ->
          Right $ pure UserCreated

        (Nothing, SetName _) ->
          Left $ UserDoesntExist UserDoesntExistError
            { message = "Received SetName command but this user doesn't exists"
            , userId
            }
        (Just _, SetName name) ->
          Right $ pure $ NameUpdated name

        (Nothing, SetRole _) ->
          Left $ UserDoesntExist UserDoesntExistError
            { message = "Received RoleSet command but this user doesn't exists"
            , userId
            }
        (Just _, SetRole role) ->
          Right $ pure $ RoleSet role
