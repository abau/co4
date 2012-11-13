{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module CO4.Unique
  ( MonadUnique(..), UniqueT, Unique, runUnique, runUniqueT, mapUnique
  , newName, newNamelike)
where

import Control.Monad.Identity
import Control.Monad.State 
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.RWS (RWST)
import Data.Monoid (Monoid)
import CO4.Names (Namelike,mapName,fromName,readName)
import CO4.Language (Name)

class (Functor m, Monad m) => MonadUnique m where
  newString :: String -> m String

newtype UniqueT m a = UniqueT (StateT Integer m a)
  deriving (Functor, Monad, MonadTrans)

newtype Unique a = Unique (UniqueT Identity a)
  deriving (Functor, Monad, MonadUnique)

instance (Functor m, Monad m) => MonadUnique (UniqueT m) where
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
newName :: (MonadUnique m, Namelike n) => n -> m Name
newName = newNamelike

-- |@newNamelike n@ returns an unique namelike with prefix @n@
newNamelike :: (MonadUnique m, Namelike n, Namelike o) => n -> m o
newNamelike prefix = newString (fromName $ nameOriginal prefix) >>= return . readName

-- |Gets the original name, i.e. the common prefix of an unique name
-- TODO: what about variable names that contain separator
nameOriginal :: Namelike n => n -> n
nameOriginal = mapName $ takeWhile (/= separator)

instance (MonadUnique m) => MonadUnique (ReaderT r m) where
  newString x = lift $ newString x

instance (MonadUnique m, Monoid w) => MonadUnique (WriterT w m) where
  newString x = lift $ newString x

instance (MonadUnique m) => MonadUnique (StateT s m) where
  newString x = lift $ newString x

instance (MonadUnique m, Monoid w) => MonadUnique (RWST r w s m) where
  newString x = lift $ newString x
