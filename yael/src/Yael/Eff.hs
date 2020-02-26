{-# LANGUAGE UndecidableInstances #-}

{-|

Yael is an effect system that models effects as records. This module
contains the core functionality needed to define and use effects.

In Yael, each effect is a record, paremeterized by a `Monad`, `m`. For example

@
data MyEffect m = MyEffect
  { _myMethod1 :: String -> m Bool
  , _myMethod2 :: m Int -> m [Int]
  }
@

Each field in an effect record is a "method" of that effect. Methods after often
functions, but they don't have to be. They can be of the form `m a` for some `a`, or
they can simply not refer to `m` at all. As seen in `MyEffect`, methods can be
first-order, meaning that `m` only appears in the result of a functional method, or
they can be higher order, meaning that `m` appears in the arguments of a functional
method. Most methods are first-order, but higher-order methods are useful to represent
effects that can wrap other effectful code, for example when working with transactional
data

-}
module Yael.Eff
  ( EffT
  , pattern EffT
  , runEffT
  , withEffT
  , withEffT'
  , localEffT
  , mapEffT
  , HasEff
  , HasEffs
  , (:+)
  , (:<>)(..)
  , Project(..)
  ) where

import Control.Lens ((^.), Lens', lens, Field1(_1), Field2(_2), (%~))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Reader as R
import Control.Exception.Safe
import UnliftIO (MonadUnliftIO(..), withUnliftIO, UnliftIO(..))
import Data.Function ((&))
import GHC.TypeLits
import GHC.Generics
import Control.Monad.Base
import Control.Monad.Trans.Control

-- | `EffT` is a monad transformer which adds an effect (or collection of effects) `f`
-- to an underlying `Monad` `m`.
newtype EffT (f :: (* -> *) -> *) (m :: * -> *) (a :: *) = MkEffT
  { unEffT :: R.ReaderT (f m) m a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadThrow
                     , MonadCatch, MonadMask, MonadPlus, Alternative, MonadBase s
                     , MonadBaseControl s)

pattern EffT r = MkEffT (R.ReaderT r)

-- | Run an effectful computation of `f` in `m` by providing a particular
-- instantiation of that effect.
runEffT
  :: forall f m a
   . EffT f m a
  -> f m
  -> m a
runEffT (EffT r) y = r y

instance MonadUnliftIO m => MonadUnliftIO (EffT f m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = EffT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runEffT r))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    EffT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runEffT r)

instance MonadTrans (EffT f) where
  lift = MkEffT . lift


-- | Lift a first-order computation that is expressible wholly through `g` into `EffT`.
-- This is most commonly used to define convient method accessors generically in
-- `EffT`.
--
-- Returning to the `MyEffect` example above, we'll see that `_myMethod1` has type
-- @MyEffect m -> String -> m Bool@. The @MyEffect m@ paremeter is cumbersome to
-- work with, so we can use `withEffT`:
--
-- @
-- myMethod1 :: (HasEff MyEffect f m) => String -> EffT f m Bool
-- myMethod1 s = withEffT $ \myEffect -> _myMethod1 myEffect s
-- @
withEffT :: (HasEff g f m) => (forall n . Monad n => g n -> n a) -> EffT f m a
withEffT use = withEffT' $ const use

-- | Lift a higher-order computation in `g` into `EffT`.
--
-- `withEffT'` is similar to `withEffT`, but it provides an additional "lowering"
-- function that is useful for working with higher order computations. For example
--
-- @
-- myMethod2 :: (HasEff MyEffect f m) => EffT f m Int -> EffT f m [Int]
-- myMethod2 m = withEffT' $ \lower myEffect -> _myMethod2 myEffect (lower m)
-- @
withEffT'
  :: (HasEff g f m)
  => (forall n . Monad n => (forall x . EffT f m x -> n x) -> g n -> n a)
  -> EffT f m a
withEffT' use = EffT $ \f -> use (\e -> runEffT e f) (f ^. prj)


-- | Modify a computational context. Useful for attaching behavior to a specific
-- scope. Compare to `R.local` in "Control.Monad.Reader"
localEffT :: Project f g => (g m -> g m) -> EffT f m a -> EffT f m a
localEffT modify (EffT r) = EffT $ \f -> r $ f & prj %~ modify


-- | Transform an entire effect context into a new context. Useful for resolving one
-- effect in a context without resolving every effect.
mapEffT
  :: Monad m
  => (f m -> g m)
  -> EffT g m a
  -> EffT f m a
mapEffT f (EffT r) = EffT $ \g -> r (f g)

-- | Typeclass for extracting a specific effect from a larger context. It should
-- generally not be necessary to implement your own instances of this class.
class Project (f :: (* -> *) -> *) g where
  prj :: Lens' (f m) (g m)

instance {-# OVERLAPPING #-} Project x x where
  prj = id

infixr :<>

-- | Combinator for composing effects.
data (a :<> b) (m :: * -> *) = a m  :<> b m
  deriving (Show, Generic)

instance Field1 ((a :<> b) m) ((a' :<> b) m) (a m) (a' m)
instance Field2 ((a :<> b) m) ((a :<> b') m) (b m) (b' m)

instance {-# OVERLAPPING #-} Project ((a :<> b)) a where
  prj = _1

instance {-# OVERLAPPABLE #-} Project b c => Project ((a :<> b)) c where
  prj = _2 . (prj @b @c)


type family MissingError x y where
  MissingError x f =
    'Text "Expected a handler for " ':<>: 'ShowType f ':<>: 'Text " to provided through `runEffT`"
    ':$$: 'Text "The handlers available are: " ':<>: ShowStack (Stacks x)

type family Stacks x where
  Stacks (f :<> g) = f ': (Stacks g)
  Stacks f = '[f]

type family ShowStack (xs :: [k]) where
  ShowStack '[x] = 'ShowType x
  ShowStack (x ': xs) = 'ShowType x ':<>: 'Text ", " ':<>: ShowStack xs

instance
  {-# OVERLAPPABLE #-}
  (TypeError (MissingError x y))  =>
  Project x y where
  prj = error "Missing implementation! This should be a type error"

-- | Type synonym expressing that an `g` is available for use within the wider
-- context `f`
type HasEff g f m = (Project f g, Monad m)

-- | Type synonym expressing that a set of effects `xs` are all available within
-- the wider context f. `xs` is expressed as a type level list, however
-- order does not matter.
type family HasEffs xs f m where
  HasEffs '[x] f m = HasEff x f m
  HasEffs (x ': xs) f m = (HasEff x f m, HasEffs xs f m)

infix 7 :+

-- | Syntactic sugar for first order computations with no additional constraints
-- on the effectful context `f` or computational context `m`.
--
-- Example:
--
-- @
-- myFunction :: String -> [Int] :+ '[MyEffect]
-- myFunction s = do
--   r1 <- myMethod1 s
--   case r1 of
--     True -> myMethod2 $ return 17
--     False -> return [17]
-- @
type (:+) v effs = forall m f . (HasEffs effs f m) => EffT f m v
