module Yael.Eff.Log where

import Yael.Eff
import Control.Monad.IO.Class

newtype Log m = Log
  { _writeLog :: String -> m ()
  }

writeLog :: String -> () :+ '[Log]
writeLog s = withEffT $ \Log{_writeLog} -> _writeLog s

stdoutLog :: (MonadIO m) => Log m
stdoutLog = Log $ liftIO . putStrLn

noLog :: Monad m => Log m
noLog = Log
  { _writeLog = const $ return ()
  }
