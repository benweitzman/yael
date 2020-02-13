{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), makeLenses)
import Yael.Eff
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad

data T m = T
  { _op' :: m Int
  , _hop' :: m Int -> m [Int]
  , _mop' :: m (Maybe Int)
  }

makeLenses ''T

op :: Int :+ '[T]
op = asksEff >>= (^. op')

hop :: HasEffs '[T] m => m Int -> m [Int]
hop mx = do
  t <- asksEff
  t ^. hop' $ mx

mop :: Maybe Int :+ '[T]
mop = asksEff >>= (^. mop')

someFunc :: Maybe (Int, [Int], [Int]) :+ '[T]
someFunc = runMaybeT $ do
  x <- lift op
  y <- lift $ hop op
  z <- MaybeT $ do
    xs <- hop op
    return $ case even $ sum xs of
      True -> Just xs
      _ -> Nothing
  return (x, y, z)

someFunc' :: (HasEffs '[T] m, MonadPlus m) => m (Int, [Int], [Int])
someFunc' = do
  x <- op
  _ <- mzero
  return (x, [x], [x])

someFunc'' :: (HasEffs '[T] m, MonadPlus m) => m (Maybe (Int, [Int], [Int]))
someFunc'' = reify someFunc'

reify :: MonadPlus m => m a -> m (Maybe a)
reify ma = (Just <$> ma) `mplus` return Nothing

reify' :: Monad m => (forall n . MonadPlus n => n a) -> m (Maybe a)
reify' = runMaybeT

main :: IO ()
main = do
  v <- someFunc
    & flip runEffT T{}
  print v
