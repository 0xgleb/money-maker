module MoneyMaker.Event
  ( Event(..)
  , NoEventsFoundError(..)
  , computeCurrentState
  , Id(..)
  )
  where

import MoneyMaker.Error

import Protolude
import qualified Data.Aeson as Aeson
import qualified Data.UUID  as UUID

-- | Custom UUID wrapper for differentiating between different kind of ids
-- This allows you to to have, for example, @Id User@ and @Id House@ and
-- the compiler will make sure that you don't mess up and use one in place
-- of another. (tag :: k) part allows you to tag it not only with types but
-- with type-level values of any "kind", for example, type-level strings
newtype Id (tag :: k)
  = Id { getId :: UUID.UUID }

class (Aeson.ToJSON event, Aeson.FromJSON event) => Event event where
  type family AggregateIdTag event :: tag
  type family EventAggregate event :: Type
  type family EventError     event :: Type

  applyEvent
    :: ( MonadUltraError m
       , EventError event `Elem` errors
       )
    => Maybe (EventAggregate event)
    -> event
    -> m errors (EventAggregate event)

data NoEventsFoundError
  = NoEventsFoundError
  deriving stock (Eq, Show)

computeCurrentState
  :: forall event errors m
   . ( Event event
     , MonadUltraError m
     , NoEventsFoundError `Elem` errors
     , EventError event `Elem` errors
     )
  => [event]
  -> m errors (EventAggregate event)
computeCurrentState = \case
  [] ->
    throwUltraError NoEventsFoundError

  (event:events) -> do
    initialState <- applyEvent Nothing event
    foldM processNextEvent initialState events

  where
    processNextEvent !currentState !event
      = applyEvent (Just currentState) event
