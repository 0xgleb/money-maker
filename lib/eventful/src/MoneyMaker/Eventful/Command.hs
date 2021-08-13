module MoneyMaker.Eventful.Command
  ( Command(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Event

import Protolude

class Eventful event => Command command event | command -> event where
  type family CommandError command :: Type

  handleCommand
    :: ( MonadUltraError m
       , CommandError command `Elem` errors
       )
    => Maybe (EventAggregate event)
    -> command
    -> m errors (EventAggregate event)
