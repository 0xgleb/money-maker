{-# LANGUAGE AllowAmbiguousTypes #-}

module MoneyMaker.Eventful.EventStore
  ( getAggregate
  , applyCommand
  , MonadEventStore(..)
  , CouldntDecodeEventError(..)

  , StorableEvent(..)
  , InMemoryEventStoreT(..)
  , runInMemoryEventStoreTWithoutErrors
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
      { id      :: !UUID.UUID
      , payload :: !Aeson.Value -- ^ JSON encoded event
      }

-- | Non-persisted in-memory event store for testing
newtype InMemoryEventStoreT (m :: Type -> Type) (errors :: [Type]) (a :: Type)
  = InMemoryEventStoreT
      { runInMemoryEventStore :: StateT [StorableEvent] (UltraExceptT m errors) a }
  deriving newtype (Functor, Applicative, Monad, MonadState [StorableEvent])

runInMemoryEventStoreTWithoutErrors
  :: Monad m
  => [StorableEvent]
  -> InMemoryEventStoreT m '[] a
  -> m (a, [StorableEvent])
runInMemoryEventStoreTWithoutErrors initialEvents (InMemoryEventStoreT action)
  = runUltraExceptTWithoutErrors $ runStateT action initialEvents

instance Monad m => MonadUltraError (InMemoryEventStoreT m) where
  throwUltraError error
    = InMemoryEventStoreT $ lift $ throwUltraError error

  catchUltraErrorMethod
    :: forall error errors a
     . InMemoryEventStoreT m (error:errors) a
    -> (error -> InMemoryEventStoreT m errors a)
    -> InMemoryEventStoreT m errors a
  catchUltraErrorMethod (InMemoryEventStoreT action) handleError = do
    currentState <- get
    (result :: Either (OneOf (error:errors)) (a, [StorableEvent])) <-
      InMemoryEventStoreT -- InMemoryEventStoreT m errors (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ lift -- StateT [StorableEvent] (UltraExceptT m errors) (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ liftToUltraExceptT -- UltraExceptT m errors (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ runUltraExceptT -- m (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ runStateT action currentState -- UltraExceptT m (error:errors) (a, [StorableEvent])

    case result of
      Right (val, _) ->
        InMemoryEventStoreT $ pure val

      Left err ->
        case getOneOf err of
          Right error -> handleError error
          Left otherErr ->
            InMemoryEventStoreT $ lift $ UltraExceptT $ ExceptT $ pure $ Left otherErr


instance Monad m => MonadEventStore (InMemoryEventStoreT m) where
  getAggregateWithProxy = getAggregateWithProxyInMemory
  applyCommandWithProxy = applyCommandWithProxyInMemory

getAggregateWithProxyInMemory
  :: ( NoEventsFoundError `Elem` errors
     , EventError event `Elem` errors
     , CouldntDecodeEventError  `Elem` errors
     , Event event
     , Monad m
     )
  => Proxy event
  -> Id (AggregateIdTag event)
  -> InMemoryEventStoreT m errors (EventAggregate event)

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
  :: forall command event errors m
   . ( Command command event
     , CouldntDecodeEventError `Elem` errors
     , CommandError command `Elem` errors
     , EventError event `Elem` errors
     , Event event
     , Monad m
     )
  => Proxy event
  -> Id (AggregateIdTag event)
  -> command
  -> InMemoryEventStoreT m errors (EventAggregate event)

applyCommandWithProxyInMemory eventProxy id command = do
  aggregate <-
    catchUltraError @NoEventsFoundError
      (Just <$> getAggregateWithProxyInMemory eventProxy id)
      (const $ pure Nothing)

  handleCommand aggregate command
