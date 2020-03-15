module Yael.Eff.Log where

import Yael.Eff
import Control.Monad.IO.Class
import System.IO
import Control.Monad.ST
import Data.STRef

newtype Log m = Log
  { _writeLog :: String -> m ()
  }

writeLog :: String -> () :+ '[Log]
writeLog s = withEffT $ \Log{_writeLog} -> _writeLog s

stdoutLog :: (MonadIO m) => Log m
stdoutLog = handleLog stdout

handleLog :: MonadIO m => Handle -> Log m
handleLog handle = Log $ liftIO . hPutStr handle

noLog :: Monad m => Log m
noLog = Log
  { _writeLog = const $ return ()
  }


withCollectLog :: (forall n . Monad n => Log n -> n a) -> (a, [String])
withCollectLog f = runST $ do
  r <- newSTRef []
  let l = Log
        { _writeLog = \s -> modifySTRef r (s:)
        }
  res <- f l
  logged <- readSTRef r
  return (res, reverse logged)
