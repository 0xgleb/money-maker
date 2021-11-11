{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module MoneyMaker.Eventful.EventStore.Persistent
  ( EventId
  , Event(..)
  , migrateAll

  , SqlEventStoreT(..)
  , runSqlEventStoreT
  , runSqlEventStoreTWithoutErrors
  )
  where

import MoneyMaker.Eventful.Command
import MoneyMaker.Eventful.Event
import MoneyMaker.Eventful.EventStore.Interface

import MoneyMaker.Based

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.UUID            as UUID
import           Database.Persist     ((==.))
import qualified Database.Persist     as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.TH  as Persist

Persist.share
  [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"]
  [Persist.persistLowerCase|
Event
    type Text
    aggregate_id Text
    payload ByteString
    deriving Show
|]
{-

The above Template Haskell block generates a type that looks something like this

data Event = Event
  { eventType         :: !Text
  , eventAggregate_id :: !Text
  , eventPayload      :: !Text
  }

As well as other types, functions, and instances that allow this type to be
converted to and from generic database types.

-}


newtype SqlEventStoreT (m :: Type -> Type) (errors :: [Type]) (a :: Type)
  = SqlEventStoreT
      { getSqlEventStoreT
          :: ReaderT Persist.ConnectionPool (UltraExceptT m errors) a
      }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Persist.ConnectionPool
    , MonadIO
    )

runSqlEventStoreT
  :: forall errors m a
   . Persist.ConnectionPool
  -> SqlEventStoreT m errors a
  -> m (Either (OneOf errors) a)

runSqlEventStoreT connectionPool (SqlEventStoreT action)
  = runUltraExceptT $ runReaderT action connectionPool

runSqlEventStoreTWithoutErrors
  :: Monad m
  => Persist.ConnectionPool
  -> SqlEventStoreT m '[] a
  -> m a

runSqlEventStoreTWithoutErrors connectionPool (SqlEventStoreT action)
  = runUltraExceptTWithoutErrors $ runReaderT action connectionPool

instance Monad m => MonadUltraError (SqlEventStoreT m) where
  throwUltraError = SqlEventStoreT . lift . throwUltraError

  catchUltraErrorMethod
    :: forall error errors a
     . SqlEventStoreT m (error:errors) a
    -> (error -> SqlEventStoreT m errors a)
    -> SqlEventStoreT m errors a
  catchUltraErrorMethod (SqlEventStoreT action) handleError = do
    connectionPool <- ask
    result :: Either (OneOf (error:errors)) a <-
      SqlEventStoreT $ lift $ liftToUltraExceptT
        $ runUltraExceptT $ runReaderT action connectionPool
      -- see full type breakdown in the in-memory instance

    case result of
      Right val ->
        SqlEventStoreT $ pure val

      Left err ->
        case getOneOf err of
          Right error -> handleError error
          Left otherErr ->
            SqlEventStoreT $ lift $ UltraExceptT $ ExceptT $ pure $ Left otherErr


instance MonadIO m => MonadEventStore (SqlEventStoreT m) where
  type MonomorphicEvent (SqlEventStoreT m) = Event

  dumpEventStore = do
    connectionPool <- ask

    liftIO $ flip Persist.runSqlPool connectionPool do
      fmap Persist.entityVal <$> Persist.selectList [] []

  getAggregateWithProxy = getAggregateWithSql
  applyCommandWithProxy = applyCommandWithSql

getAggregateWithSql
  :: ( NoEventsFoundError `Elem` errors
     , CouldntDecodeEventError  `Elem` errors
     , EventError event `Elem` errors
     , Eventful event
     , MonadIO m
     )
  => Proxy event
  -> Id (EventName event)
  -> SqlEventStoreT m errors (EventAggregate event)

getAggregateWithSql (_ :: Proxy event) (Id uuid) = do
  connectionPool <- ask

  decodedEvents <- liftIO $ flip Persist.runSqlPool connectionPool $ do
    -- TODO: add caching
    rawEvents <-
      fmap Persist.entityVal
        <$> Persist.selectList [EventAggregate_id ==. UUID.toText uuid] []

    pure $ sequence $ Aeson.decode . BSL.fromStrict . eventPayload <$> rawEvents

  case decodedEvents of
    Nothing ->
      -- TODO: throw a more useful error
      throwUltraError $ CouldntDecodeEventError "Couldn't decode smth, idk what"
    Just events ->
      computeCurrentState @event events


applyCommandWithSql
  :: forall command event errors m
   . ( Command command event
     , CouldntDecodeEventError `Elem` errors
     , CommandError command `Elem` errors
     , EventError event `Elem` errors
     , Eventful event
     , MonadIO m
     )
  => Proxy event
  -> Id (EventName event)
  -> command
  -> SqlEventStoreT m errors (EventAggregate event)

applyCommandWithSql eventProxy aggregateId command = do
  -- Get the current aggregate if it exists
  maybeAggregate :: Maybe (EventAggregate event) <-
    catchUltraError @NoEventsFoundError
      (Just <$> getAggregateWithSql eventProxy aggregateId)
      (const $ pure Nothing)

  -- Use the command's handleCommand method to get what events should be added
  (headEvent :| tailEvents) <-
    handleCommand aggregateId maybeAggregate command & \case
      Right events -> pure events
      Left error   -> throwUltraError error

  let allEvents = headEvent : tailEvents

  -- Get a non-maybe aggregate
  nextAggregate <-
    applyEvent maybeAggregate headEvent

  -- Apply the rest of the new events to the aggregate
  aggregate <-
    foldM -- :: (b -> a -> m b) -> b -> t a -> m b
      (\agg nextEvent -> applyEvent (Just agg) nextEvent)
      nextAggregate
      tailEvents

  connectionPool <- ask

  liftIO $ flip Persist.runSqlPool connectionPool $ do
    -- Insert new events into the database
    -- TODO: ensure events go in the right order
    void $ Persist.insertMany $ allEvents <&> \event ->
      Event
        { eventType
            = toS $ symbolVal $ Proxy @(EventName event)

        , eventAggregate_id
            = UUID.toText $ getId aggregateId

        , eventPayload
            = BSL.toStrict $ Aeson.encode event
        }

    pure aggregate
