{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Rename
  (rename, renameM, renameMap, renameList)
where

import           Control.Monad.Reader
import           Control.Monad.Identity
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           CO4.Algorithms.Instantiator
import           CO4.Language

type MappingM m = Name -> m Name
type Mapping    = Name ->   Name

newtype Instantiator m a = Instantiator { runInstantiator :: ReaderT (MappingM m) m a }
  deriving (Monad, MonadReader (MappingM m))

instance Monad m => MonadInstantiator (Instantiator m) where
  instantiateName name = do
    f <- ask
    lift $ f name

instance MonadTrans Instantiator where
  lift = Instantiator . lift

-- |Renames by a monadic mapping
renameM :: (Monad m, Instantiable a) => MappingM m -> a -> m a
renameM mapping a = runReaderT (runInstantiator $ instantiate a) mapping

-- |Renames by a mapping
rename :: Instantiable a => Mapping -> a -> a
rename mapping = runIdentity . renameM (return . mapping)

-- |Renames by a map
renameMap :: Instantiable a => M.Map Name Name -> a -> a
renameMap map = 
  let mapping n = fromMaybe n $ M.lookup n map
  in
    rename mapping

-- |Renames by a list of pairs
renameList :: Instantiable a => [(Name,Name)] -> a -> a
renameList map = renameMap $ M.fromList map
