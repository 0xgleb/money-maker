module MoneyMaker.Eventful.EventStore.InMemory
  ( InMemoryEventStoreT(..)
  , runInMemoryEventStoreT
  , runInMemoryEventStoreTWithoutErrors

  , toStorableEvent
  , StorableEvent(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Command
import MoneyMaker.Eventful.Event
import MoneyMaker.Eventful.EventStore.Interface

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.UUID  as UUID


data StorableEvent
  = StorableEvent
      { id      :: !UUID.UUID
      , payload :: !Aeson.Value -- ^ JSON encoded event
      }
  deriving stock (Eq, Show)

toStorableEvent
  :: Eventful event
  => Id (EventName event)
  -> event
  -> StorableEvent
toStorableEvent aggregateId
  = StorableEvent (getId aggregateId) . Aeson.toJSON

-- TODO: refactor so that it takes m :: [Type] -> Type -> Type instead of using
-- UltraExceptT. This way it will be more composable
-- | Non-persisted in-memory event store for testing
newtype InMemoryEventStoreT (m :: Type -> Type) (errors :: [Type]) (a :: Type)
  = InMemoryEventStoreT
      { getInMemoryEventStoreT :: StateT [StorableEvent] (UltraExceptT m errors) a }
  deriving newtype (Functor, Applicative, Monad, MonadState [StorableEvent])

runInMemoryEventStoreT
  :: Monad m
  => [StorableEvent] -- ^ Initial state of the event store
  -> InMemoryEventStoreT m errors a
  -> m (Either (OneOf errors) (a, [StorableEvent]))

runInMemoryEventStoreT genesisEvents (InMemoryEventStoreT action)
  = runUltraExceptT $ runStateT action genesisEvents

runInMemoryEventStoreTWithoutErrors
  :: Monad m
  => [StorableEvent]
  -> InMemoryEventStoreT m '[] a
  -> m (a, [StorableEvent])
runInMemoryEventStoreTWithoutErrors initialEvents (InMemoryEventStoreT action)
  = runUltraExceptTWithoutErrors $ runStateT action initialEvents

instance Monad m => MonadUltraError (InMemoryEventStoreT m) where
  throwUltraError = InMemoryEventStoreT . lift . throwUltraError

  catchUltraErrorMethod
    :: forall error errors a
     . InMemoryEventStoreT m (error:errors) a
    -> (error -> InMemoryEventStoreT m errors a)
    -> InMemoryEventStoreT m errors a
  catchUltraErrorMethod (InMemoryEventStoreT action) handleError = do
    currentState <- get
    result :: Either (OneOf (error:errors)) (a, [StorableEvent]) <-
      InMemoryEventStoreT -- InMemoryEventStoreT m errors (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ lift -- StateT [StorableEvent] (UltraExceptT m errors) (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ liftToUltraExceptT -- UltraExceptT m errors (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ runUltraExceptT -- m (Either (OneOf (error:errors)) (a, [StorableEvent]))
        $ runStateT action currentState -- UltraExceptT m (error:errors) (a, [StorableEvent])

    case result of
      Right (val, eventStore) -> do
        put eventStore
        pure val

      Left err ->
        case getOneOf err of
          Right error -> handleError error
          Left otherErr ->
            InMemoryEventStoreT $ lift $ UltraExceptT $ ExceptT $ pure $ Left otherErr


instance Monad m => MonadEventStore (InMemoryEventStoreT m) where
  type MonomorphicEvent (InMemoryEventStoreT m) = StorableEvent

  dumpEventStore = get

  getAggregateWithProxy = getAggregateWithProxyInMemory
  applyCommandWithProxy = applyCommandWithProxyInMemory

getAggregateWithProxyInMemory
  :: ( NoEventsFoundError `Elem` errors
     , EventError event `Elem` errors
     , CouldntDecodeEventError  `Elem` errors
     , Eventful event
     , Monad m
     )
  => Proxy event
  -> Id (EventName event)
  -> InMemoryEventStoreT m errors (EventAggregate event)

getAggregateWithProxyInMemory (_ :: Proxy event) (Id uuid) = do
  allEvents <- get

  let relevantEvents
        = sequence
        $ Aeson.fromJSON . payload
            <$> filter ((== uuid) . id) allEvents

  case relevantEvents of
    Aeson.Error err ->
      throwUltraError $ CouldntDecodeEventError err
    Aeson.Success events ->
      computeCurrentState @event events


applyCommandWithProxyInMemory
  :: forall command event errors m
   . ( Command command event
     , CouldntDecodeEventError `Elem` errors
     , CommandError command `Elem` errors
     , EventError event `Elem` errors
     , Eventful event
     , Monad m
     )
  => Proxy event
  -> Id (EventName event)
  -> command
  -> InMemoryEventStoreT m errors (EventAggregate event)

applyCommandWithProxyInMemory eventProxy aggregateId command = do
  -- Get the current aggregate if it exists
  maybeAggregate :: Maybe (EventAggregate event) <-
    catchUltraError @NoEventsFoundError
      (Just <$> getAggregateWithProxyInMemory eventProxy aggregateId)
      (const $ pure Nothing)

  -- Use the command's handleCommand method to get what events should be added
  headEvent :| tailEvents <-
    handleCommand aggregateId maybeAggregate command & \case
      Right events -> pure events
      Left error   -> throwUltraError error

  let allNewEvents = headEvent : tailEvents

  -- Get a non-maybe aggregate
  nextAggregate <-
    applyEvent maybeAggregate headEvent

  -- Apply the rest of the new events to the aggregate
  aggregate <-
    foldM -- :: (b -> a -> m b) -> b -> t a -> m b
      (\agg nextEvent -> applyEvent (Just agg) nextEvent)
      nextAggregate
      tailEvents

  -- Prepare to store the new events in the right format
  let newStorableEvents :: [StorableEvent]
        = toStorableEvent aggregateId <$> allNewEvents

  -- Update the state with the new events and return the new aggregate
  state $ (aggregate,) . (<> newStorableEvents)
