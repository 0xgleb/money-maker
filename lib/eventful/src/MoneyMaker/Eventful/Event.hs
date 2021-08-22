module MoneyMaker.Eventful.Event
  ( Eventful(..)
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
newtype Id (tag :: Symbol)
  = Id { getId :: UUID.UUID }

class
  ( Aeson.ToJSON event
  , Aeson.FromJSON event
  , KnownSymbol (EventName event)
  ) => Eventful event
  where
    type family EventName event      :: Symbol
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
   . ( Eventful event
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
