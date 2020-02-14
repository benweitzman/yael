# Yael 

Yet Another Effect Library

## Intro

Yael is a simple but powerful effect library built around modeling effects with records. It is inspired by the `ReaderT IO` pattern, but uses records instead of typeclasses to describe effects. 

Yael encourages building pure, high level effects out of lower level effects to enable application design and testing.

## Example

```haskell

import Yael.Eff
import Database.Persist
import Models.Person

data Sql m = Sql
  { runSql :: forall a. ReaderT SqlBackend m a -> m a -- persistent's transaction model
  }
  
data User m = User
  { createUser :: Person -> m PersonId
  , getUsers :: m [Entity Person]
  } 
  
sqlUser :: (MonadIO m) => Sql m -> User m
sqlUser Sql{runSql} = User 
  { createUser = \p -> runSql $ insert p
  , getUsers = runSql $ selectList [] []
  }
  
program :: (HasEffs '[User, Sql] m) => m (Either String Person)
... TBD
```



## Design goals

 ### Low boilerplate 
 
Writing and using effects should not *require* template haskell. Because effects are just records, there's
very little overhead in creating new effects. No need to define any typeclasses to get started.
 
 ### Fast
 
I don't know much about this but I'm told that using records and/or typeclasses is faster than free monads
 
 ### Purity
 
Records are parameterized by an `m`, which is chosen when the record is constructed, not when it's used. This makes it 
easy to write pure application code that doesn't allow any `IO`! 

Because records are plain ol' Haskell values, it's easy to create many different implementations of an effect to use in testing code. They can even be modified and overriden! 
 
