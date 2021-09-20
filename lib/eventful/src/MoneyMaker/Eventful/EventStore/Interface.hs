{-# LANGUAGE AllowAmbiguousTypes #-}

module MoneyMaker.Eventful.EventStore.Interface
  ( getAggregate
  , applyCommand
  , MonadEventStore(..)
  , CouldntDecodeEventError(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Command
import MoneyMaker.Eventful.Event

import qualified Prelude
import           Protolude

getAggregate
  :: forall event errors m
   . ( Eventful event
     , MonadEventStore m
     , MonadUltraError m
     , NoEventsFoundError `Elem` errors
     , EventError event `Elem` errors
     , CouldntDecodeEventError  `Elem` errors
     )
  => Id (EventName event)
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
     , Eventful event
     )
  => Id (EventName event)
  -> command
  -> m errors (EventAggregate event)
applyCommand = applyCommandWithProxy $ Proxy @event

data CouldntDecodeEventError
  = CouldntDecodeEventError Prelude.String
  deriving stock (Show)

class MonadUltraError m => MonadEventStore (m :: [Type] -> Type -> Type) where
  type MonomorphicEvent m :: Type

  dumpTheEventStore :: m errors [MonomorphicEvent m]

  getAggregateWithProxy
    :: ( NoEventsFoundError `Elem` errors
       , EventError event `Elem` errors
       , CouldntDecodeEventError `Elem` errors
       , Eventful event
       )
    => Proxy event
    -> Id (EventName event)
    -> m errors (EventAggregate event)

  applyCommandWithProxy
    :: ( Command command event
       , EventError event `Elem` errors
       , CouldntDecodeEventError `Elem` errors
       , CommandError command `Elem` errors
       , EventError event `Elem` errors
       , Eventful event
       )
    => Proxy event
    -> Id (EventName event)
    -> command
    -> m errors (EventAggregate event)
