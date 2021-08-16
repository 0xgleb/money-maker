{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Flexible type-safe error handling mechanism
module MoneyMaker.Error
  ( MonadUltraError(..)
  , catchUltraError
  , catchVoid
  , type Elems
  , type (++)
  , Elem
  , OneOf(..)
  , getOneOf
  , UltraEither(..)

  , UltraExceptT(..)
  , runUltraExceptT
  , runUltraExceptTWithoutErrors
  , liftToUltraExceptT
  )
  where

import Protolude
import qualified Prelude

-- | Type class interface for monadic types that are parametrised
-- by possible errors & can throw and catch them
class
  ( forall errors. Monad (m errors) )
  => MonadUltraError (m :: [Type] -> Type -> Type)
  where
    -- | If the error you want to throw is in the list of errors that you can
    -- fail with then you can throw that error
    throwUltraError
      :: error `Elem` errors
      => error
      -> m errors a

    -- | You want to use @MonadUltraError@ with polymorphic @errors@. And
    -- instead of explicitly specifying the list of types your code can fail
    -- with, you should write code that works for all lists of errors that have
    -- the errors you need inside them. This allows us not to convert error
    -- lists when calling different functions and it allows us not to care
    -- about the order of the errors in the error list, which allows us to
    -- pattern match on the first element of the list in @catchUltraErrorMethod@
    -- without worrying about that error being in a different place in the list
    -- (because we can pick the order arbitrarily as we our functions care that
    -- certain errors are in the list, and we don't care where in the list those
    -- errors are)
    catchUltraErrorMethod
      :: m (error:errors) a
      -> (error -> m errors a)
      -> m errors a

-- | We can't put an explicit @forall@ in the type class method, which means
-- that if you want to use TypeApplications language extension to specify the
-- exact types you want the function to use (like the type of the error you want
-- to catch or the list of errors), you will need to supply the types in some
-- random order. This is why we have this function that explicitly lists type
-- variables and the order in which they should be supplied by TypeApplications
--
-- > catchUltraError @YourError @[AnotherError, YourError, ThirdError]
catchUltraError
  :: forall error errors m a
   . MonadUltraError m
  => m (error:errors) a
  -> (error -> m errors a)
  -> m errors a
catchUltraError = catchUltraErrorMethod

catchVoid
  :: MonadUltraError m
  => m (Void:errors) a
  -> m errors a
catchVoid = flip catchUltraError absurd

-- | @OneOf@ is parametrised by a list of types and contains a value
-- of one of the types from the list
data OneOf (xs :: [Type]) where
  -- | @ThisOne@ allows you to create a @OneOf@ parametrised by any type-level
  -- list that starts with the type of the argument you pass to @ThisOne@
  -- e.g. @ThisOne (5 :: Integer) :: [Integer, String, Bool]@
  ThisOne :: x -> OneOf (x : xs)

  -- | If the type of the value that you need to be in @OneOf@ isn't the first
  -- element of the type-level list then you need to use @Other@ to adjust it
  -- to the right position
  Other :: OneOf xs -> OneOf (y : xs)

-- TODO: implement this. required for unit tests
instance Prelude.Show (OneOf xs) where
  show = Prelude.error "implement OneOf show"

-- TODO: implement this. required for unit tests
instance Prelude.Eq (OneOf xs) where
  _ == _ = Prelude.error "implement Eq OneOf"

-- | In order to implement @MonadUltraError@, we need to be able to check what's
-- inside a @OneOf@ so that if it has the error we are trying to handle, we can
-- get the value and handle it, and if it doesn't have the error, then we want
-- to get a @OneOf@ with a reduced type-level list without that type.
--
-- We are also using FunctionalDependencies language extension here for saying
-- @x xsWith -> xsWithout@, which simply means that if we know type @x@ and
-- @xsWith@ then that implies @xsWithout@.
class
  GetOneOf (x :: Type) (xsWith :: [Type]) (xsWithout :: [Type])
    | x xsWith -> xsWithout
  where
    getOneOf
      :: OneOf xsWith
      -> Either (OneOf xsWithout) x

-- The base case is pretty simple. If the first type in the type-level list
-- is the type we are looking for then we can just pattern match and return
-- the element we are looking for or retagged @OneOf@
instance GetOneOf x (x:xs) xs where
  getOneOf = \case
    ThisOne x   -> Right x
    Other other -> Left other

-- The recursive step basically says that if you have @GetOneOf@ for some x
-- and some lists then if you add an element to both of those lists, you can
-- still @getOneOf@. The INCOHERENT pragma is needed to ensure GHC can
-- distinguish between this instance and the above instance. INCOHERENT pragma
-- simply tells GHC that if multiple instances match and it needs to pick one
-- then it shouldn't pick this one. You should have at most one INCOHERENT
-- otherwise if GHC needs to pick from multiple INCOHERENT instances, it will
-- pick one completely randomly
instance {-# INCOHERENT #-}
  GetOneOf x xsWith xsWithout
  => GetOneOf x (y:xsWith) (y:xsWithout)
  where
    getOneOf = \case
      ThisOne x   -> Left (ThisOne x)
      Other other -> first Other $ getOneOf other

-- | Type family for supporting multiple errors with one constraint
-- e.g. if your function can fail with @NetworkError@ and @ZeroDivisionError@
-- then your type will look something like this
--
-- > function
-- >   :: ( [NetworkError, ZeroDivisionError] `Elems` errors
-- >      , MonadUltraError m
-- >      )
-- >   => someArgs
-- >   -> m errors a
type family Elems (monoErrors :: [Type]) (polyErrors :: [Type]) :: Constraint where
  -- we need the ' in '[] so that Haskell can distinguish between an empty
  -- type-level list and the normal list type. The () on the right is not
  -- unit type but rather an empty constraint
  '[] `Elems` errors = ()

  -- just recursively propagating the Elem constraint
  (e:es) `Elems` errors =
    ( e `Elem` errors -- ensure the current error is in the errors list
    , es `Elems` errors -- ensure the same for the following elements
    )

type family (++) (listA :: [a]) (listB :: [a]) :: [a] where
  '[] ++ listB = listB

  (x : xs) ++ listB = x : (xs ++ listB)

-- | We need @Elem@ for two reasons: one is to assert that an error is in the
-- error list and the other is for creating a value of type @OneOf xs@ from
-- a value of type @x@ if type @x@ is in the type-level list @xs@
class Elem (x :: Type) (xs :: [Type]) where
  mkOneOf :: x -> OneOf xs

-- How would we implement value-level @elem@? The easiest way is to write
-- a recursive function using pattern matching
--
-- elem :: Eq a => a -> [a] -> Bool
-- elem y (x:xs) = if y == x then True else elem y xs
-- elem []       = False
--
-- That's nearly identical to what we would do if we used type families to
-- implement Elem and pretty similar to the type class approach that we are using
instance Elem x (x : xs) where
  mkOneOf = ThisOne

-- GHC can't distinguish between this instance and the above instance.
-- Which is why we need {-# INCOHERENT #-}
-- INCOHERENT pragma tells GHC that if it's picking between two instances,
-- it should not pick this instance. There should only be at most one
-- INCOHERENT instance, otherwise GHC will randomly pick between different
-- INCOHERENT instances
instance {-# INCOHERENT #-} Elem x xs => Elem x (y : xs) where
  mkOneOf = Other . mkOneOf

-- | Newtype around @Either@ that
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

-- | ExceptT parametrised by errors it can have
newtype UltraExceptT (m :: Type -> Type) (errors :: [Type]) (a :: Type)
  = UltraExceptT { getUltraExceptT :: ExceptT (OneOf errors) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runUltraExceptT :: UltraExceptT m errors a -> m (Either (OneOf errors) a)
runUltraExceptT = runExceptT . getUltraExceptT

liftToUltraExceptT :: Monad m => m a -> UltraExceptT m errors a
liftToUltraExceptT
  = UltraExceptT . lift

-- | This function allows unwrapping UltraExceptT without any errors
-- if the error list is empty
runUltraExceptTWithoutErrors :: Monad m => UltraExceptT m '[] a -> m a
runUltraExceptTWithoutErrors UltraExceptT{..} = do
  result <- runExceptT getUltraExceptT
  case result of
    Right rightResult ->
      pure rightResult
    Left error ->
      case error of -- an empty case statement because there are no values of OneOf '[]

instance Monad m => MonadUltraError (UltraExceptT m) where
  throwUltraError error
    = UltraExceptT $ throwError $ mkOneOf error

  catchUltraErrorMethod
    :: forall error errors a
     . UltraExceptT m (error:errors) a
    -> (error -> UltraExceptT m errors a)
    -> UltraExceptT m errors a
  catchUltraErrorMethod (UltraExceptT failableAction) handleError = do
    result <- liftToUltraExceptT $ runExceptT failableAction
    case result of
      Right val ->
        pure val
      Left (error :: OneOf (error:errors)) ->
        case getOneOf error of
          Left (err :: OneOf errors)  -> UltraExceptT $ throwError err
          Right (err :: error) -> handleError err
