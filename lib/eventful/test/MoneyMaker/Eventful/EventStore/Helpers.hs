module MoneyMaker.Eventful.EventStore.Helpers
  ( TestEventStoreProcedureErrors
  , testEventStoreProcedure
  , userAggregateId
  )
  where

import MoneyMaker.Eventful.EventSpec

import MoneyMaker.Error
import MoneyMaker.Eventful

import Protolude

type TestEventStoreProcedureErrors
  = [ UserEventError
    , CouldntDecodeEventError
    , NoEventsFoundError
    , CommandError UserCommand
    ]

userAggregateId :: Id "user"
userAggregateId = Id @"user" [uuid|123e4666-e89b-12d3-a456-666614174000|]

testEventStoreProcedure
  :: forall errors m
   . ( MonadEventStore m
     , TestEventStoreProcedureErrors `Elems` errors
     )
  => m errors User

testEventStoreProcedure = do
  void $ applyCommand userAggregateId CreateUser
  void $ applyCommand userAggregateId $ SetName "Creator"
  void $ applyCommand userAggregateId $ SetRole Genius

  catchUltraError throwAnError $ const $ pure ()

  void $ applyCommand userAggregateId $ SetName "Gleb"
  void $ applyCommand userAggregateId $ SetRole Engineer

  getAggregate @UserEvent userAggregateId

  where
    throwAnError :: m (() : errors) ()
    throwAnError = throwUltraError ()
