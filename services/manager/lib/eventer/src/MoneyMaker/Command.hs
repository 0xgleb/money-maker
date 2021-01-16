module MoneyMaker.Command
  ( Command(..)
  )
  where

import MoneyMaker.Event

import Protolude

class Event event => Command command event | command -> event where
  type family CommandError command :: Type

  handleCommand
    :: Maybe (EventAggregate event)
    -> command
    -> Either (CommandError command) (EventAggregate event)
