module Environment
  ( Environment(..)
  , Mode(..)
  , getEnvironment
  )
  where

import MoneyMaker.Based

import qualified Data.ByteString.Char8 as BS
import qualified LoadEnv
import qualified Paths_exe             as Path
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

data Environment
  = Environment
      { dbName       :: ByteString
      , user         :: ByteString
      , password     :: ByteString
      , cbAPIKey     :: ByteString
      , cbPassphrase :: ByteString
      , mode         :: Mode
      }

data Mode
  = TestMode -- ^ use sandbox environment
  | ProdMode -- ^ use prod environment
  deriving stock (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = do
  LoadEnv.loadEnvFromAbsolute =<< Path.getDataFileName "../.env"

  maybeDbName   <- Env.lookupEnv "POSTGRES_DB"
  maybeUser     <- Env.lookupEnv "POSTGRES_USER"
  maybePassword <- Env.lookupEnv "POSTGRES_PASSWORD"

  maybeCBAPIKey <- Env.lookupEnv "CB_ACCESS_KEY"
  maybeCBPassphrase <- Env.lookupEnv "CB_ACCESS_PASSPHRASE"

  ( BS.pack -> dbName
    , BS.pack -> user
    , BS.pack -> password
    , BS.pack -> cbAPIKey
    , BS.pack -> cbPassphrase
    ) <-
    case (maybeDbName, maybeUser, maybePassword, maybeCBAPIKey, maybeCBPassphrase) of
      (Just db, Just user, Just password, Just apiKey, Just passphrase) ->
        pure (db, user, password, apiKey, passphrase)

      (Nothing, _, _, _, _) -> do
        putStrLn @Text "POSTGRES_DB environment variable is missing"
        Exit.exitFailure

      (_, Nothing, _, _, _) -> do
        putStrLn @Text "POSTGRES_USER environment variable is missing"
        Exit.exitFailure

      (_, _, Nothing, _, _) -> do
        putStrLn @Text "POSTGRES_PASSWORD environment variable is missing"
        Exit.exitFailure

      (_, _, _, Nothing, _) -> do
        putStrLn @Text "CB_ACCESS_KEY environment variable is missing"
        Exit.exitFailure

      (_, _, _, _, Nothing) -> do
        putStrLn @Text "CB_ACCESS_PASSPHRASE environment variable is missing"
        Exit.exitFailure

  pure Environment
    { mode = TestMode
    , ..
    }
