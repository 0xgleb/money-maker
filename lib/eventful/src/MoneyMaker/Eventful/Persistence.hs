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

import Protolude

import qualified Database.Persist     as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.TH  as Persist

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
Event
    type Text
    event Text
    deriving Show
|]

newtype SqlEventStoreT (m :: Type -> Type) (errors :: [Type]) (a :: Type)
  = SqlEventStoreT { runSqlEventStoreT :: ReaderT Persist.ConnectionPool (UltraExceptT m errors) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Persist.ConnectionPool)

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
      -- see full type breakdown in the InMemoryEventStoreT instance

    case result of
      Right val ->
        SqlEventStoreT $ pure val

      Left err ->
        case getOneOf err of
          Right error -> handleError error
          Left otherErr ->
            SqlEventStoreT $ lift $ UltraExceptT $ ExceptT $ pure $ Left otherErr
