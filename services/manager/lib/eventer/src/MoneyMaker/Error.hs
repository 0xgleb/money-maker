{-# LANGUAGE QuantifiedConstraints #-}

module MoneyMaker.Error
  ( MonadUltraError(..)
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

    catchUltraError
      :: ( error `Elem` errors
         , GetOneOf error errors
         )
      => m errors a
      -> (error -> m (errors `Without` error) a)
      -> m (errors `Without` error) a



type Elem (x :: Type) (xs :: [Type])
  = ( (x `BoolElem` xs) ~ True
    , InjectOneOf x xs
    )

type family BoolElem (x :: k) (xs :: [k]) :: Bool where
  -- need the tick in front of [] so that Haskell can
  -- distinguish between list type and empty type-level list
  x `BoolElem` '[]      = False
  x `BoolElem` (x : xs) = True
  x `BoolElem` (_ : xs) = x `BoolElem` xs

type family Without (xs :: [k]) (x :: k) :: [k] where
  '[]      `Without` x = '[]
  (x : xs) `Without` x = xs `Without` x
  (y : xs) `Without` x = y : (xs `Without` x)



data OneOf (xs :: [Type]) where
  ThisOne :: x -> OneOf (x : xs)
  Other   :: OneOf xs -> OneOf (y : xs)

instance Prelude.Show (OneOf xs) where
  show = Prelude.error "implement OneOf show"

instance Prelude.Eq (OneOf xs) where
  _ == _ = Prelude.error "implement Eq OneOf"

class GetOneOf (x :: Type) (xs :: [Type]) where
  getOneOf
    :: x `Elem` xs
    => OneOf xs
    -> Either (OneOf (xs `Without` x)) x

instance (xs `Without` x) ~ xs => GetOneOf x (x:xs) where
  getOneOf = \case
    ThisOne x   -> Right x
    Other other -> Left other

instance {-# INCOHERENT #-}
  ( x `Elem` xs
  , GetOneOf x xs
  , (y : (xs `Without` x)) ~ ((y:xs) `Without` x)
  ) => GetOneOf x (y:xs)
  where
    getOneOf = \case
      ThisOne x   -> Left (ThisOne x :: OneOf (y:(xs `Without` x)))
      Other other -> first Other $ getOneOf other

-- class (x `BoolElem` xs) ~ True => InjectOneOf (x :: Type) (xs :: [Type]) where
class InjectOneOf (x :: Type) (xs :: [Type]) where
  injectOneOf :: x -> OneOf xs

instance InjectOneOf x (x : xs) where
  injectOneOf = ThisOne

instance {-# INCOHERENT #-}
  ( (x `BoolElem` xs) ~ True
  , InjectOneOf x xs
  )
  => InjectOneOf x (y : xs)
  where
    injectOneOf = Other . injectOneOf

newtype UltraEither (errors :: [Type]) (a :: Type)
  = UltraEither { getUltraEither :: Either (OneOf errors) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadUltraError UltraEither where
  throwUltraError error
    = UltraEither $ Left $ injectOneOf error

  catchUltraError (UltraEither (Right val)) _
    = UltraEither $ Right val

  catchUltraError (UltraEither (Left error)) handleError
    = case getOneOf error of
        Left err  -> UltraEither $ Left err
        Right err -> handleError err
