module MoneyMaker.Eventful.EventStore.PersistentSpec
  ( spec
  )
  where

import MoneyMaker.Eventful.EventSpec          hiding (spec)
import MoneyMaker.Eventful.EventStore.Helpers

import MoneyMaker.Eventful

import Protolude
import Test.Hspec

import qualified Control.Monad.Logger    as Logger
import qualified Data.Pool               as Pool
import qualified Database.Persist.Sql    as Persist
import qualified Database.Persist.Sqlite as Sqlite

spec :: Spec
spec = do
  describe "MonadEventStore SqlEventStoreT" $ do
    it "executing testEventStoreProcedure results in the expected user aggregate" $ do
      result <- Logger.runNoLoggingT $ Sqlite.withSqlitePool ":memory:" 1 $ \connectionPool -> do
        Pool.withResource connectionPool
          $ runReaderT $ Persist.runMigration migrateAll

        runSqlEventStoreT connectionPool
          $ testEventStoreProcedure @TestEventStoreProcedureErrors

      result `shouldBe` (Right exampleUser)
