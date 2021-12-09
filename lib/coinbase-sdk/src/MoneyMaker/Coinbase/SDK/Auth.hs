{-# LANGUAGE QuantifiedConstraints #-}

module MoneyMaker.Coinbase.SDK.Auth
  ( signMessage
  , Method(..)
  , Path(..)
  , SecretDecodingError(..)
  )
  where

import MoneyMaker.Based

import qualified Crypto.Hash.SHA256     as Hash
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Time              as Time
import qualified Timestamp

data Method
  = Post
  | Get

methodToBS :: Method -> ByteString
methodToBS = \case
  Post -> "POST"
  Get  -> "GET"

newtype Path
  = Path { unwrapPath :: ByteString }

newtype SecretDecodingError
  = SecretDecodingError [Char]

signMessage
  :: ( MonadUltraError m
     , SecretDecodingError `Elem` errors
     , forall allErrors. MonadIO (m allErrors)
     )
  => ByteString
  -> Method
  -> Path
  -> Aeson.Value
  -> m errors ByteString
signMessage secret method path payload = do
  -- get the timestamp
  timestamp <-
    (`div` 1000) . Timestamp.timestampMicroSecondsInt64 . Timestamp.utcTimeTimestamp
        <$> liftIO Time.getCurrentTime

  -- create the prehash string by concatenating required parts
  let message :: ByteString
        =  encodeUtf8 (show timestamp)
        <> methodToBS method
        <> unwrapPath path
        <> BSL.toStrict (Aeson.encode payload)

  -- decode the base64 secret
  key <- case Base64.decode secret of
    Left error ->
      throwUltraError $ SecretDecodingError error

    Right key ->
      pure key

  -- sign the require message with the hmac
  -- and finally base64 encode the result
  pure $ Hash.hmac key message
