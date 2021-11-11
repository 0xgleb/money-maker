module MoneyMaker.Eventful.Command
  ( Command(..)
  )
  where

import MoneyMaker.Eventful.Event

import MoneyMaker.Based

class Eventful event => Command command event | command -> event where
  type family CommandError command :: Type

  handleCommand
    :: Id (EventName event) -- event ID so that it can be used in errors
    -> Maybe (EventAggregate event)
    -> command
    -> Either (CommandError command) (NonEmpty event)
