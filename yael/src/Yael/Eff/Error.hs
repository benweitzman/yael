module Yael.Eff.Error where

import Yael.Eff
import qualified Control.Monad.Except as E
import qualified Control.Exception.Safe as Ex

data Error e m = Error
  { _throw :: forall a . e -> m a
  , _catch :: forall a . m a -> (e -> m a) -> m a
  }

throw :: e -> a :+ '[Error e]
throw e = withEffT $ \Error{_throw} -> _throw e

catch
  :: (HasEff (Error e) f m)
  => EffT f m a
  -> (e -> EffT f m a)
  -> EffT f m a
catch m f = withEffT' $ \lower Error{_catch} ->
  _catch (lower m) (lower . f)


type UncheckedError = Error Ex.SomeException

mtlError :: E.MonadError e m => Error e m
mtlError = Error
  { _throw = E.throwError
  , _catch = E.catchError
  }

exceptionError
  :: (Ex.Exception e, Ex.MonadThrow m, Ex.MonadCatch m)
  => Error e m
exceptionError = Error
  { _throw = Ex.throw
  , _catch = Ex.catch
  }


throwAny :: Ex.Exception e => e -> a :+ '[UncheckedError]
throwAny = throw . Ex.toException

try :: (HasEff (Error e) f m) => EffT f m a -> EffT f m (Either e a)
try m = (Right <$> m) `catch` (return . Left)
