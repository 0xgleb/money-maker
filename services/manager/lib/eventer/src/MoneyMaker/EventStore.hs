{-# LANGUAGE AllowAmbiguousTypes #-}

module MoneyMaker.EventStore
  ( getAggregate
  , MonadEventStore(..)
  , EventStoreError(..)

  , StorableEvent(..)
  , InMemoryEventStore(..)
  )
  where

import MoneyMaker.Event
import MoneyMaker.Error

import Protolude

import qualified Prelude
import qualified Data.Aeson as Aeson
import qualified Data.UUID  as UUID

getAggregate
  :: forall event errors m
   . ( Event event
     , MonadEventStore m
     , MonadUltraError m
     , AggregationError `Elem` errors
     , EventError event `Elem` errors
     , EventStoreError  `Elem` errors
     )
  => Id (AggregateIdTag event)
  -> m errors (EventAggregate event)
getAggregate = getAggregateWithProxy $ Proxy @event


data EventStoreError
  = CouldntDecodeEvent Prelude.String

class (MonadUltraError m)
  => MonadEventStore (m :: [Type] -> Type -> Type)
  where
    getAggregateWithProxy
      :: ( AggregationError `Elem` errors
         , EventError event `Elem` errors
         , EventStoreError  `Elem` errors
         , Event event
         )
      => Proxy event
      -> Id (AggregateIdTag event)
      -> m errors (EventAggregate event)

  -- command
  --   :: Command command event
  --   => Id (AggregateIdTag event)
  --   -> command
  --   -> m (Either (AggregationError (Event)))

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

  catchUltraError (InMemoryEventStore action) handleError = do
    store <- get
    let UltraEither result = runStateT action store
    case result of
      Right (val, _) ->
        InMemoryEventStore $ pure val

      Left err ->
        case getOneOf err of
          Right error   -> handleError error
          Left otherErr -> InMemoryEventStore $ lift $ UltraEither $ Left otherErr


instance MonadEventStore InMemoryEventStore where
  getAggregateWithProxy
    :: ( AggregationError `Elem` errors
       , EventError event `Elem` errors
       , EventStoreError  `Elem` errors
       , Event event
       )
    => Proxy event
    -> Id (AggregateIdTag event)
    -> InMemoryEventStore errors (EventAggregate event)

  getAggregateWithProxy (Proxy :: Proxy event) (Id uuid) = do
    allEvents <- get

    let relevantEvents
          = sequence
          $ Aeson.fromJSON . payload
              <$> filter ((== uuid) . id) allEvents

    case relevantEvents of
      Aeson.Error err -> throwUltraError $ CouldntDecodeEvent err
      Aeson.Success events -> do
        computeCurrentState @event events
