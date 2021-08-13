module MoneyMaker.Eventful.Command
  ( Command(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Event

import Protolude

class
  ( Eventful event
  -- , KnownSymbol (CommandName command)
  ) => Command command event | command -> event
  where
    -- type family CommandName command :: Symbol

    type family CommandError command :: Type

    handleCommand
      :: ( MonadUltraError m
        , CommandError command `Elem` errors
        )
      => Maybe (EventAggregate event)
      -> command
      -> m errors (NonEmpty event)
