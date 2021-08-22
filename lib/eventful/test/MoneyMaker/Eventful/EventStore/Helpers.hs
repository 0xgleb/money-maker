module MoneyMaker.Eventful.EventStore.Helpers
  ( TestEventStoreProcedureErrors
  , testEventStoreProcedure
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
    ]
    ++ CommandErrors UserCommand

testEventStoreProcedure
  :: forall errors m
   . ( MonadEventStore m
     , TestEventStoreProcedureErrors `Elems` errors
     )
  => m errors User

testEventStoreProcedure = do
  let userId = Id @"user" [uuid|123e4666-e89b-12d3-a456-666614174000|]

  void $ applyCommand userId CreateUser
  void $ applyCommand userId $ SetName "Creator"
  void $ applyCommand userId $ SetRole Genius
  void $ applyCommand userId $ SetName "Gleb"
  void $ applyCommand userId $ SetRole Engineer

  getAggregate @UserEvent userId
