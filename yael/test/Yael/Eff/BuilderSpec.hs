{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoStrictData #-}

module Yael.Eff.BuilderSpec where

import Control.Lens
import Yael.Eff
import Yael.Eff.Builder
import Yael.Eff.Log
import           Test.Hspec
import           Test.QuickCheck
import GHC.Generics
import Data.Function
import Data.Functor.Identity
import Control.Monad.Trans

data X m = X
  { _x :: m Bool
  }

x :: Bool :+ '[X]
x = withEffT _x

data Y m = Y
  { _y :: m Int
  }

y :: Int :+ '[Y]
y = withEffT _y

xY :: HasEffs '[X] f m => f m -> Y m
xY f = Y
  { _y = flip runEffT f $ do
      v <- x
      return $ if v then 10 else 11
  }

data Z m = Z
  { _z :: m Int -> m Bool
  }

z :: HasEffs '[Z] f m => EffT f m Int -> EffT f m Bool
z i = withEffT' $ \lower Z{_z} -> _z (lower i)

xyZ :: HasEffs '[X, Y] f m => f m -> Z m
xyZ f = Z
  { _z = \i -> flip runEffT f $ do
      y' <- y
      q <- (y' *) <$> lift i
      (even q ||) <$> x
  }

data XStack f = XStack
  { xStackX :: Effect f X
  } deriving (Generic, EffStack)

data XYStack f = XYStack
  { xyStackX :: Effect f X
  , xyStackY :: Effect f Y
  } deriving (Generic, EffStack)

data XYZStack f = XYZStack
  { xyzStackX :: Effect f X
  , xyzStackY :: Effect f Y
  , xyzStackZ :: Effect f Z
  } deriving (Generic, EffStack)

data XYZLogStack f = XYZLogStack
  { xyzLogStackX :: Effect f X
  , xyzLogStackY :: Effect f Y
  , xyzLogStackZ :: Effect f Z
  , xyzLogStackLog :: Effect f Log
  } deriving (Generic, EffStack)


logAround :: HasEffs '[Log] f m => f m -> Y m -> Y m
logAround f yEff = Y
  { _y = flip runEffT (yEff :<> f) $ do
      writeLog "before"
      resp <- y
      writeLog "after"
      return resp
  }

spec :: Spec
spec = do
  describe "runEffTWith" $ do
    it "should evaluate simple effects" $ property $ \expectedVal -> do
      let val = runIdentity $ x &
            runEffTWith XStack
            { xStackX = const $ X { _x = return expectedVal }
            }
      val `shouldBe` expectedVal

    it "can do Y" $ do
      let val = runIdentity $ flip runEffT (xY $ X { _x = return False }) y
      val `shouldBe` 11

    it "can resolve effect dependencies" $ do
      let val = runIdentity $ (,) <$> x <*> y
            & runEffTWith XYStack
            { xyStackX = const $ X { _x = return True }
            , xyStackY = xY
            }
      val `shouldBe` (True, 10)

      let val = runIdentity $ (,,) <$> x <*> y <*> (z y)
            & runEffTWith XYZStack
            { xyzStackX = const $ X { _x = return True }
            , xyzStackY = xY
            , xyzStackZ = xyZ
            }
      val `shouldBe` (True, 10, True)

    it "can handle wrapping" $ do
      let (val, log) = withCollectLog $ \logF ->
            (,,) <$> x <*> y <*> z y
            & runEffTWith XYZLogStack
            { xyzLogStackX = const $ X { _x = return True }
            , xyzLogStackY = logAround `wrapping` xY
            , xyzLogStackZ = xyZ
            , xyzLogStackLog = const logF
            }
      val `shouldBe` (True, 10, True)
      log `shouldBe` ["before", "after", "before", "after", "before", "after"]

    it "can handle double wrapping" $ do
      let (val, log) = withCollectLog $ \logF ->
            (,,) <$> x <*> y <*> z y
            & runEffTWith XYZLogStack
            { xyzLogStackX = const $ X { _x = return True }
            , xyzLogStackY = logAround `wrapping` logAround `wrapping` xY
            , xyzLogStackZ = xyZ
            , xyzLogStackLog = const logF
            }
      val `shouldBe` (True, 10, True)
      log `shouldBe` [ "before"
                     , "before"
                     , "after"
                     , "after"
                     , "before"
                     , "before"
                     , "after"
                     , "after"
                     , "before"
                     , "before"
                     , "after"
                     , "after"
                     ]
