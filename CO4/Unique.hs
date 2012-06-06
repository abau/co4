{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module CO4.Unique
  ( MonadUnique(..), UniqueT, Unique, runUnique, runUniqueT, mapUnique
  , newName, newName')
where

import Control.Monad.Identity
import Control.Monad.State 
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.RWS (RWST)
import Data.Monoid (Monoid)
import CO4.Language (Name(..))

class Monad m => MonadUnique m where
  newString :: String -> m String

newtype UniqueT m a = UniqueT (StateT Integer m a)
  deriving (Functor, Monad, MonadTrans)

newtype Unique a = Unique (UniqueT Identity a)
  deriving (Functor, Monad, MonadUnique)

instance Monad m => MonadUnique (UniqueT m) where
  newString prefix = UniqueT $ do
    i <- get
    modify (+1) 
    return $ concat [prefix, separator:[], show i]

separator = '_'

runUniqueT :: Monad m => UniqueT m a -> m a
runUniqueT (UniqueT u) = evalStateT u 0

runUnique :: Unique a -> a
runUnique (Unique u) = runIdentity $ runUniqueT u

mapUnique :: Monad m => Unique a -> UniqueT m a
mapUnique (Unique (UniqueT (StateT (run)))) = 
  UniqueT $ StateT $ return . runIdentity . run

-- |@newName n@ returns an unique name with prefix @n@
newName :: MonadUnique m => Name -> m Name
newName name = newName' $ (\name -> case name of
                                Name n -> n
                                TypedName n _ -> n
                          ) $ nameOriginal name

newName' :: MonadUnique m => String -> m Name
newName' prefix = newString prefix >>= return . Name

-- |Gets the original name, i.e. the common prefix of an unique name
-- TODO: what about variable names that contain separator
nameOriginal :: Name -> Name
nameOriginal (Name n)        = Name $ takeWhile (/= separator) n
nameOriginal (TypedName n s) = TypedName (takeWhile (/= separator) n) s

instance (MonadUnique m) => MonadUnique (ReaderT r m) where
  newString x = lift $ newString x

instance (MonadUnique m, Monoid w) => MonadUnique (WriterT w m) where
  newString x = lift $ newString x

instance (MonadUnique m) => MonadUnique (StateT s m) where
  newString x = lift $ newString x

instance (MonadUnique m, Monoid w) => MonadUnique (RWST r w s m) where
  newString x = lift $ newString x
