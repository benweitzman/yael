module Yael.EffSpec where

import Control.Monad
import Yael.Eff
import Yael.Eff.Data
import           Test.Hspec
import Test.QuickCheck
import Data.Functor.Identity

-- | Nullary effect with zero methods.
data Null m = Null

newtype Unit a m = Unit
  { _unit :: m a
  }

unit :: a :+ '[Unit a]
unit = withEffT $ \Unit{_unit} -> _unit

newtype Return m = Return
  { _return :: forall a . a -> m a
  }

newtype IdT m = IdT
  { _idT :: forall a . m a -> m a
  }

newtype Tuple m = Tuple
  { _tuple :: forall a b . m a -> m b -> m (a, b)
  }

newtype X m = X
  { _x :: forall a . (m Int -> m a) -> m a
  }

spec :: Spec
spec = do
  describe "runEffT" $ do
    it "should evaluate the proper value" $ property $
      \(x :: Int) -> runEffT (return x) Null == Identity x

    it "should provide access to the effect via EffT" $ property $
      \(x :: Int) -> runEffT (EffT $ return . _getData) (Data x) == Identity x

    it "should provide access to the effect via liftEffT" $ property $
      \(x :: Int) -> runEffT (liftEffT $ return . _getData) (Data x) == Identity x

  describe "withEffT" $ do
    it "should lift constant methods into EffT" $ property $
      \(x :: Int) -> runEffT (withEffT $ \Null -> return x) Null == Identity x

    it "should lift unitary methods into EffT" $ property $
      \(x :: Int) ->
        let u = Unit
              { _unit = return x
              }
        in runEffT (withEffT $ \Unit{_unit} -> _unit) u == Identity x

    it "should lift methods with arguments into EffT" $ property $
      \(x :: Int) ->
        let r = Return
              { _return = return
              }
        in runEffT (withEffT $ \Return{_return} -> _return x) r == Identity x

  describe "withEffT'" $ do
    it "should lift higher order methods into EffT" $ property $
      \(x :: Int) ->
        let i = IdT
              { _idT = id
              }

            m :: Monad m => a -> EffT f m a
            m x = EffT $ const $ return x
        in runEffT (withEffT' $ \lower IdT{_idT} -> _idT $ lower $ m x) i == Identity x

    it "should lift higher order methods with multiple into EffT" $ property $
      \(x :: Int) (y :: Int) ->
        let t = Tuple
              { _tuple = liftM2 (,)
              }

            m :: Monad m => a -> EffT f m a
            m x = EffT $ const $ return x

            f :: Monad m => EffT Tuple m (Int, Int)
            f = withEffT' $ \lower Tuple{_tuple} -> _tuple (lower $ m x) (lower $ m y)

        in runEffT f t == Identity (x, y)

  describe "withEffT''" $ do
    it "should lift methods with `m a` in positive postion into EffT" $ property $
      \(x :: Int) (y :: Int) ->
        let xx = X
              { _x = \f -> f $ return x
              }

            f :: Monad m => (EffT X m Int -> EffT X m a) -> EffT X m a
            f mm = withEffT'' $ \lower raise X{_x} -> _x $ lower . mm . raise

            a :: Monad m => EffT X m Int -> EffT X m Int
            a mi = do
              i <- mi
              return $ i + y
        in runEffT (f a) xx == Identity (x + y)


  describe "localEffT" $ do
    it "should overload an effect locally" $ property $
      \(x :: Int) (y :: Int) ->
        let initial :: Monad m => Unit Int m
            initial = Unit $ return x

            m = do
              a <- unit
              b <- localEffT (const $ Unit $ return y) unit
              c <- unit
              return (a, b, c)
        in runEffT m initial == Identity (x, y, x)

  describe "mapEffT" $ do
    it "should discharge an effect constraint" $ property $
      \(x :: Int) (y :: String) ->
        let m :: (Int, String) :+ '[Unit Int, Unit String]
            m = (,) <$> unit <*> unit

            -- n doesn't have a `Unit Int` constraint but uses it internally
            n :: (Int, String) :+ '[Unit String]
            n = mapEffT (Unit (return x) :<>) m

        in runEffT n (Unit $ return y) == Identity (x, y)
