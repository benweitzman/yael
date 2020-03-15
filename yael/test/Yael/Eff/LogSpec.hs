module Yael.Eff.LogSpec where

import           Data.Functor.Identity
import           Test.Hspec
import           Test.QuickCheck
import           Yael.Eff
import           Yael.Eff.Log

spec :: Spec
spec = do
  describe "noLog" $ do
    it "allows for a pure run" $ property $
      \(x :: Int) -> runEffT (writeLog $ show x) noLog == Identity ()


  describe "withCollectLog" $ do
    it "allows for a pure run with logged results" $ property $
      \(x :: Int) ->
        let m :: Int :+ '[Log]
            m = do
              writeLog "start"
              a <- return x
              writeLog "end"
              return a

            result = withCollectLog $ \logE -> runEffT m logE

        in result == (x, ["start", "end"])
