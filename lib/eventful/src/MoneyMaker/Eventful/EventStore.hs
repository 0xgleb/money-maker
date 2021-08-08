{-# LANGUAGE AllowAmbiguousTypes #-}

module MoneyMaker.Eventful.EventStore
  ( getAggregate
  , applyCommand
  , MonadEventStore(..)
  , CouldntDecodeEventError(..)

  , StorableEvent(..)
  , InMemoryEventStore(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Command
import MoneyMaker.Eventful.Event

import Protolude

import qualified Prelude
import qualified Data.Aeson as Aeson
import qualified Data.UUID  as UUID

getAggregate
  :: forall event errors m
   . ( Event event
     , MonadEventStore m
     , MonadUltraError m
     , NoEventsFoundError `Elem` errors
     , EventError event `Elem` errors
     , CouldntDecodeEventError  `Elem` errors
     )
  => Id (AggregateIdTag event)
  -> m errors (EventAggregate event)
getAggregate = getAggregateWithProxy $ Proxy @event

applyCommand
  :: forall event errors command m
   . ( Command command event
     , MonadEventStore m
     , EventError event `Elem` errors
     , CouldntDecodeEventError `Elem` errors
     , CommandError command `Elem` errors
     , EventError event `Elem` errors
     , Event event
     )
  => Id (AggregateIdTag event)
  -> command
  -> m errors (EventAggregate event)
applyCommand = applyCommandWithProxy $ Proxy @event

data CouldntDecodeEventError
  = CouldntDecodeEventError Prelude.String

class MonadUltraError m => MonadEventStore (m :: [Type] -> Type -> Type) where
  getAggregateWithProxy
    :: ( NoEventsFoundError `Elem` errors
       , EventError event `Elem` errors
       , CouldntDecodeEventError `Elem` errors
       , Event event
       )
    => Proxy event
    -> Id (AggregateIdTag event)
    -> m errors (EventAggregate event)

  applyCommandWithProxy
    :: ( Command command event
       , EventError event `Elem` errors
       , CouldntDecodeEventError `Elem` errors
       , CommandError command `Elem` errors
       , EventError event `Elem` errors
       , Event event
       )
    => Proxy event
    -> Id (AggregateIdTag event)
    -> command
    -> m errors (EventAggregate event)

data StorableEvent
  = StorableEvent
      { id      :: UUID.UUID
      , payload :: Aeson.Value -- ^ JSON encoded event
      }

-- | Non-persisted in-memory event store for testing
newtype InMemoryEventStore (errors :: [Type]) (a :: Type)
  = InMemoryEventStore
      { runInMemoryEventStore :: StateT [StorableEvent] (UltraEither errors) a }
  deriving newtype (Functor, Applicative, Monad, MonadState [StorableEvent])


instance MonadUltraError InMemoryEventStore where
  throwUltraError error
    = InMemoryEventStore $ lift $ throwUltraError error

  catchUltraErrorMethod (InMemoryEventStore action) handleError = do
    UltraEither result <- runStateT action <$> get
    case result of
      Right (val, _) ->
        InMemoryEventStore $ pure val

      Left err ->
        case getOneOf err of
          Right error -> handleError error
          Left otherErr ->
            InMemoryEventStore $ lift $ UltraEither $ Left otherErr


instance MonadEventStore InMemoryEventStore where
  getAggregateWithProxy = getAggregateWithProxyInMemory
  applyCommandWithProxy = applyCommandWithProxyInMemory

getAggregateWithProxyInMemory
  :: ( NoEventsFoundError `Elem` errors
      , EventError event `Elem` errors
      , CouldntDecodeEventError  `Elem` errors
      , Event event
      )
  => Proxy event
  -> Id (AggregateIdTag event)
  -> InMemoryEventStore errors (EventAggregate event)

getAggregateWithProxyInMemory (_ :: Proxy event) (Id uuid) = do
  allEvents <- get

  let relevantEvents
        = sequence
        $ Aeson.fromJSON . payload
            <$> filter ((== uuid) . id) allEvents

  case relevantEvents of
    Aeson.Error err -> throwUltraError $ CouldntDecodeEventError err
    Aeson.Success events -> do
      computeCurrentState @event events

applyCommandWithProxyInMemory
  :: forall command event errors
   . ( Command command event
     , CouldntDecodeEventError `Elem` errors
     , CommandError command `Elem` errors
     , EventError event `Elem` errors
     , Event event
     )
  => Proxy event
  -> Id (AggregateIdTag event)
  -> command
  -> InMemoryEventStore errors (EventAggregate event)

applyCommandWithProxyInMemory eventProxy id command = do
  aggregate <-
    catchUltraError @NoEventsFoundError
      (Just <$> getAggregateWithProxyInMemory eventProxy id)
      (const $ pure Nothing)

  handleCommand aggregate command
