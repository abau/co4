{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module CO4.Unique
  ( MonadUnique(..), UniqueT, Unique, withUnique, withUniqueT
  , newName, newNamelike, originalName, liftListen, liftPass)
where

import           Control.Applicative (Applicative)
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as State
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Writer
import           Control.Monad.RWS (RWST)
import           Control.Monad.Signatures (Listen,Pass)
import           Language.Haskell.TH.Syntax (Quasi(..))
import           CO4.Names (Namelike,mapName,fromName,readName)
import           CO4.Language (Name)

class (Monad m) => MonadUnique m where
  newString :: String -> m String
  newInt    :: m Int

newtype UniqueT m a = UniqueT { runUniqueT :: StateT Int m a }
  deriving (Monad, Functor, Applicative, MonadTrans)

newtype Unique a = Unique (UniqueT Identity a)
  deriving (Monad, Functor, Applicative, MonadUnique)

instance (Monad m) => MonadUnique (UniqueT m) where
  newString prefix = UniqueT $ do
    i <- get
    modify (+1) 
    return $ concat [prefix, separator:[], show i]

  newInt = UniqueT $ do
    i <- get
    modify (+1) 
    return i

separator = '_'

withUniqueT :: Monad m => UniqueT m a -> m a
withUniqueT (UniqueT u) = evalStateT u 0

withUnique :: Unique a -> a
withUnique (Unique u) = runIdentity $ withUniqueT u

-- |@newName n@ returns an unique name with prefix @n@
newName :: (MonadUnique m, Namelike n) => n -> m Name
newName = newNamelike

-- |@newNamelike n@ returns an unique namelike with prefix @n@
newNamelike :: (MonadUnique m, Namelike n, Namelike o) => n -> m o
newNamelike prefix = newString (fromName $ originalName prefix) >>= return . readName

-- |Gets the original name, i.e. the common prefix of an unique name
-- TODO: what about variable names that contain separator
originalName :: Namelike n => n -> n
originalName = mapName $ takeWhile (/= separator)

instance (MonadUnique m) => MonadUnique (ReaderT r m) where
  newString = lift . newString 
  newInt    = lift   newInt

instance (MonadUnique m, Monoid w) => MonadUnique (WriterT w m) where
  newString = lift . newString
  newInt    = lift   newInt

instance (MonadUnique m) => MonadUnique (StateT s m) where
  newString = lift . newString
  newInt    = lift   newInt

instance (MonadUnique m, Monoid w) => MonadUnique (RWST r w s m) where
  newString = lift . newString
  newInt    = lift   newInt

instance (MonadIO m) => MonadIO (UniqueT m) where
  liftIO = lift . liftIO 

instance (Quasi m, Applicative m) => Quasi (UniqueT m) where
  qNewName            = lift . qNewName
  qReport a b         = lift $ qReport a b
  qRecover a b        =        qRecover a b
  qLookupName a b     = lift $ qLookupName a b
  qReify              = lift . qReify
  qReifyInstances a b = lift $ qReifyInstances a b
  qReifyRoles         = lift . qReifyRoles
  qReifyAnnotations   = lift . qReifyAnnotations
  qReifyModule        = lift . qReifyModule
  qLocation           = lift   qLocation
  qRunIO              = lift . qRunIO
  qAddDependentFile   = lift . qAddDependentFile
  qAddTopDecls        = lift . qAddTopDecls
  qAddModFinalizer    = lift . qAddModFinalizer
  qGetQ               = lift   qGetQ
  qPutQ               = lift . qPutQ

liftListen :: Monad m => Listen w m (a,Int) -> Listen w (UniqueT m) a
liftListen listen = UniqueT . State.liftListen listen . runUniqueT

liftPass :: Monad m => Pass w m (a,Int) -> Pass w (UniqueT m) a
liftPass pass = UniqueT . State.liftPass pass . runUniqueT
