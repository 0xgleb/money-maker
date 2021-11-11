module MoneyMaker.Eventful.Event
  ( Eventful(..)
  , NoEventsFoundError(..)
  , computeCurrentState
  , Id(..)
  )
  where

import MoneyMaker.Based

import qualified Data.Aeson as Aeson

class
  ( Aeson.ToJSON event
  , Aeson.FromJSON event
  , KnownSymbol (EventName event)
  ) => Eventful event
  where
    type family EventName      event = (name :: Symbol) | name -> event
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
