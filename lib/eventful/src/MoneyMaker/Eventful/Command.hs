module MoneyMaker.Eventful.Command
  ( Command(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Event

import Protolude

class Eventful event => Command command event | command -> event where
  type family CommandErrors command :: [Type]

  handleCommand
    :: ( MonadUltraError m
       , CommandErrors command `Elems` errors
       )
    => Id (EventName event)
    -> Maybe (EventAggregate event)
    -> command
    -> m errors (NonEmpty event)
