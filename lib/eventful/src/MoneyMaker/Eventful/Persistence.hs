{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MoneyMaker.Eventful.Persistence
  ( EventId
  , Event(..)
  , migrateAll

  , SqlEventStoreT(..)
  )
  where

import MoneyMaker.Error
import MoneyMaker.Eventful.Command
import MoneyMaker.Eventful.Event
import MoneyMaker.Eventful.EventStore

import Protolude

import qualified Data.Aeson           as Aeson
import qualified Data.UUID            as UUID
import           Database.Persist     ((==.))
import qualified Database.Persist     as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.TH  as Persist
import qualified Data.ByteString.Lazy as BSL

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
Event
    type Text
    aggregate_id Text
    payload ByteString
    deriving Show
|]
-- data Event = Event
--   { eventType         :: !Text
--   , eventAggregate_id :: !Text
--   , eventPayload      :: !Text
--   }


newtype SqlEventStoreT (m :: Type -> Type) (errors :: [Type]) (a :: Type)
  = SqlEventStoreT { runSqlEventStoreT :: ReaderT Persist.ConnectionPool (UltraExceptT m errors) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Persist.ConnectionPool, MonadIO)

instance Monad m => MonadUltraError (SqlEventStoreT m) where
  throwUltraError = SqlEventStoreT . lift . throwUltraError

  catchUltraErrorMethod
    :: forall error errors a
     . SqlEventStoreT m (error:errors) a
    -> (error -> SqlEventStoreT m errors a)
    -> SqlEventStoreT m errors a
  catchUltraErrorMethod (SqlEventStoreT action) handleError = do
    conn <- ask
    result :: Either (OneOf (error:errors)) a <-
      SqlEventStoreT $ lift $ liftToUltraExceptT $ runUltraExceptT $ runReaderT action conn
      -- see full type breakdown in the in-memory instance

    case result of
      Right val ->
        SqlEventStoreT $ pure val

      Left err ->
        case getOneOf err of
          Right error -> handleError error
          Left otherErr ->
            SqlEventStoreT $ lift $ UltraExceptT $ ExceptT $ pure $ Left otherErr


-- instance Monad m => MonadEventStore (SqlEventStoreT m) where
--   getAggregateWithProxy = getAggregateWithSql
--   applyCommandWithProxy = applyCommandWithSql

getAggregateWithSql
  :: ( NoEventsFoundError `Elem` errors
     , CouldntDecodeEventError  `Elem` errors
     , EventError event `Elem` errors
     , Eventful event
     , MonadIO m
     )
  => Proxy event
  -> Id (AggregateIdTag event)
  -> SqlEventStoreT m errors (EventAggregate event)

getAggregateWithSql (_ :: Proxy event) (Id uuid) = do
  conn <- ask

  decodedEvents <- liftIO $ flip Persist.runSqlPool conn $ do
    rawEvents <-
      fmap Persist.entityVal
        <$> Persist.selectList [EventAggregate_id ==. UUID.toText uuid] []

    pure $ sequence $ Aeson.decode . BSL.fromStrict . eventPayload <$> rawEvents

  case decodedEvents of
    Nothing ->
      throwUltraError $ CouldntDecodeEventError "Couldn't decode smth, idk what"
    Just events ->
      computeCurrentState @event events
