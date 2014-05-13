{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Config 
where

import Control.Applicative (Applicative)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Language.Haskell.TH.Syntax (Quasi(..))
import CO4.Unique (UniqueT)

data Config  = NoSatchmo
             | Dump FilePath
             | InstantiationDepth Int
             | ImportPrelude
             | Profile
             | Cache
             | HideSource
             | OnlyAllocators
             | NoAllocators
             deriving (Eq,Show)

type Configs      = [Config]

class (Monad m) => MonadConfig m where
  configs :: m Configs

newtype ConfigurableT m a = ConfigurableT { run :: ReaderT Configs m a }
  deriving (Monad, Functor, Applicative, MonadReader Configs, MonadTrans)

configurable :: Configs -> ConfigurableT m a -> m a
configurable configs c = runReaderT (run c) configs

is :: (MonadConfig m,Monad m) => Config -> m Bool
is c = liftM (elem c) configs

when' :: (MonadConfig m) => Config -> m () -> m ()
when' c doThis = is c >>= \case True  -> doThis 
                                False -> return ()

whenNot' :: (MonadConfig m) => Config -> m () -> m ()
whenNot' c doThis = is c >>= \case False -> doThis 
                                   True  -> return ()

fromConfigs :: MonadConfig m => (Configs -> a) -> m a
fromConfigs f = configs >>= return . f

defaultInstantiationDepth = 10
instantiationDepth :: Configs -> Int
instantiationDepth cs = case cs of
  (InstantiationDepth d):_ -> d
  []                       -> defaultInstantiationDepth
  _                        -> instantiationDepth $ tail cs

dumpTo :: Configs -> Maybe FilePath
dumpTo cs = case cs of
  (Dump fp):_ -> Just fp
  []          -> Nothing
  _           -> dumpTo $ tail cs

instance (Monad m) => MonadConfig (ConfigurableT m) where
  configs = ask

instance (MonadConfig m) => MonadConfig (UniqueT m) where
  configs = lift configs

instance (MonadConfig m) => MonadConfig (StateT a m) where
  configs = lift configs

instance (MonadConfig m) => MonadConfig (ReaderT a m) where
  configs = lift configs

instance (MonadConfig m,Monoid a) => MonadConfig (WriterT a m) where
  configs = lift configs

instance (MonadIO m) => MonadIO (ConfigurableT m) where
  liftIO = lift . liftIO

instance (Quasi m, Applicative m) => Quasi (ConfigurableT m) where
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
