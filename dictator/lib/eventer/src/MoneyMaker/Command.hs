module MoneyMaker.Command
  ( Command(..)
  )
  where

import MoneyMaker.Event
import MoneyMaker.Error

import Protolude

class Event event => Command command event | command -> event where
  type family CommandError command :: Type

  handleCommand
    :: ( MonadUltraError m
       , CommandError command `Elem` errors
       )
    => Maybe (EventAggregate event)
    -> command
    -> m errors (EventAggregate event)
