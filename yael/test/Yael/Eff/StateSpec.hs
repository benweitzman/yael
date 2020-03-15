{-# LANGUAGE TupleSections #-}

module Yael.Eff.StateSpec where

import Control.Lens
import           Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck
import           Yael.Eff
import           Yael.Eff.State
import qualified Control.Monad.State as MTL
import Data.IORef

spec :: Spec
spec = do
  let m :: (Int, Int) :+ '[State Int]
      m = do
        a <- get
        put (a + 1)
        b <- get
        return (a, b)

  describe "withState" $ do
    it "allows for a pure run" $ property $
      \(x :: Int) ->
        let result = withState x $ \state -> runEffT m state
        in result == ((x, x + 1), x + 1)

  describe "mtlState" $ do
    it "can be converted to mtl" $ property $
      \(x :: Int) ->
        let result = flip MTL.runState x $ runEffT m mtlState
        in result == ((x, x + 1), x + 1)

  describe "shareState" $ do
    it "allows multiple state types" $ property $
      \(x :: Int) (y :: String) ->
        let m' :: (Int, String) :+ '[State Int, State String]
            m' = do
              a <- get
              put (a + 1)
              b <- get
              put (b ++ "!")
              return (a, b)

            result = withState (x, y) $ \state -> do
              let intState = shareState _1 state
                  stringState = shareState _2 state
              runEffT m' (intState :<> stringState)
        in result == ((x, y), (x + 1, y ++ "!"))

  describe "ioRefState" $ do
    it "works" $ property $
      \(x :: Int) -> ioProperty $ do
        ref <- newIORef x
        result <- runEffT m (ioRefState ref)
        return $ result == (x, x + 1)
