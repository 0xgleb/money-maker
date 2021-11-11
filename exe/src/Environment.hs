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
      { dbName   :: ByteString
      , user     :: ByteString
      , password :: ByteString
      , mode     :: Mode
      }

data Mode
  = TestMode -- ^ use sandbox environment
  | ProdMode -- ^ use prod environment
  deriving stock (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = do
  LoadEnv.loadEnvFromAbsolute =<< Path.getDataFileName "../database.env"

  maybeDbName   <- Env.lookupEnv "POSTGRES_DB"
  maybeUser     <- Env.lookupEnv "POSTGRES_USER"
  maybePassword <- Env.lookupEnv "POSTGRES_PASSWORD"

  (BS.pack -> dbName, BS.pack -> user, BS.pack -> password) <-
    case (maybeDbName, maybeUser, maybePassword) of
      (Just db, Just user, Just password) ->
        pure (db, user, password)

      (Nothing, _, _) -> do
        putStrLn @Text "POSTGRES_DB environment variable is missing"
        Exit.exitFailure

      (_, Nothing, _) -> do
        putStrLn @Text "POSTGRES_USER environment variable is missing"
        Exit.exitFailure

      (_, _, Nothing) -> do
        putStrLn @Text "POSTGRES_PASSWORD environment variable is missing"
        Exit.exitFailure

  pure Environment
    { mode = TestMode
    , ..
    }
