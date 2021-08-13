{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MoneyMaker.Eventful.Persistence
  ( dummy
  )
  where

dummy :: ()
dummy = ()

-- import qualified Database.Persist    as Persist
-- import qualified Database.Persist.TH as Persist

-- Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
-- Event
--     type String
--     event String
--     deriving Show
-- |]
