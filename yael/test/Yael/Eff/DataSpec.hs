module Yael.Eff.DataSpec where

import Control.Monad
import Yael.Eff
import Yael.Eff.Data
import           Test.Hspec
import Test.QuickCheck
import Data.Functor.Identity

spec :: Spec
spec = do
  describe "Data" $ do
    it "returns a constant value" $ property $
      \(x :: Int) -> runEffT getData (Data x) == Identity x

  describe "localData" $ do
    it "overloads a constant within a scope" $ property $
      \(x :: Int) (y :: Int) ->
        let m = do
              a <- getData
              b <- localData (+ y) getData
              c <- getData
              return (a, b, c)
        in runEffT m (Data x) == Identity (x, x + y, x)
