{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MoneyMaker.Eventful.Persistence
  ( EventId
  , Event(..)
  , migrateAll
  )
  where

import Protolude

import qualified Database.Persist    as Persist
import qualified Database.Persist.TH as Persist

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
Event
    type Text
    event Text
    deriving Show
|]
