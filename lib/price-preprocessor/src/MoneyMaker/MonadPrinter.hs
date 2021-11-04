{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.MonadPrinter
  ( MonadPrinter(..)
  )
  where

import Protolude

class (forall errors. Monad (m errors)) => MonadPrinter (m :: [Type] -> Type -> Type) where
  say :: Text -> m errors ()

  showAndSay :: Show a => a -> m errors ()
  showAndSay = say . show

  showAndSayWithTitle :: Show a => Text -> a -> m errors ()
  showAndSayWithTitle title value = do
    say $ "\n" <> title
    showAndSay value
    say ""

instance {-# OVERLAPPABLE #-} (forall errors. MonadIO (m errors)) => MonadPrinter m where
  say = putStrLn
