{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module MoneyMaker.Error
  ( MonadUltraError(..)
  , catchUltraError
  , Elem
  , OneOf(..)
  , getOneOf
  , UltraEither(..)
  )
  where

import Protolude
import qualified Prelude

class
  ( forall errors. Monad (m errors) )
  => MonadUltraError (m :: [Type] -> Type -> Type)
  where
    throwUltraError
      :: error `Elem` errors
      => error
      -> m errors a

    catchUltraErrorMethod
      :: m (error:errors) a
      -> (error -> m errors a)
      -> m errors a

-- separate function with explicit forall for easy type applications
catchUltraError
  :: forall error errors m a
   . MonadUltraError m
  => m (error:errors) a
  -> (error -> m errors a)
  -> m errors a
catchUltraError = catchUltraErrorMethod

data OneOf (xs :: [Type]) where
  ThisOne :: x -> OneOf (x : xs)
  Other   :: OneOf xs -> OneOf (y : xs)

instance Prelude.Show (OneOf xs) where
  show = Prelude.error "implement OneOf show"

instance Prelude.Eq (OneOf xs) where
  _ == _ = Prelude.error "implement Eq OneOf"

class GetOneOf (x :: Type) (xsWith :: [Type]) (xsWithout :: [Type]) | x xsWith -> xsWithout where
  getOneOf
    :: OneOf xsWith
    -> Either (OneOf xsWithout) x

instance GetOneOf x (x:xs) xs where
  getOneOf = \case
    ThisOne x   -> Right x
    Other other -> Left other

instance {-# INCOHERENT #-} GetOneOf x xsWith xsWithout => GetOneOf x (y:xsWith) (y:xsWithout) where
  getOneOf = \case
    ThisOne x   -> Left (ThisOne x)
    Other other -> first Other $ getOneOf other

class Elem (x :: Type) (xs :: [Type]) where
  mkOneOf :: x -> OneOf xs

instance Elem x (x : xs) where
  mkOneOf = ThisOne

instance {-# INCOHERENT #-} Elem x xs => Elem x (y : xs) where
  mkOneOf = Other . mkOneOf

newtype UltraEither (errors :: [Type]) (a :: Type)
  = UltraEither { getUltraEither :: Either (OneOf errors) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadUltraError UltraEither where
  throwUltraError error
    = UltraEither $ Left $ mkOneOf error

  catchUltraErrorMethod (UltraEither (Right val)) _
    = UltraEither $ Right val

  catchUltraErrorMethod (UltraEither (Left error)) handleError
    = case getOneOf error of
        Left err  -> UltraEither $ Left err
        Right err -> handleError err
