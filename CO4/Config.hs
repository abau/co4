{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Config 
where

import Control.Applicative (Applicative)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Language.Haskell.TH.Syntax (Quasi(..))
import CO4.Unique (UniqueT)

import System.IO (stderr, hPutStrLn)

data Config  = Verbose
             {-
             | Degree Int
             | DegreeLoop Int
             | Metric String
             | NoRaml
             -}
             | NoSatchmo
             | DumpAfter String FilePath
             | DumpAll FilePath
             | InstantiationDepth Int
             | ImportPrelude
             | KeepTmp
             | UndefinedSize String
             | MakeFormula
             | Profile
             | Cache
             deriving (Eq,Show)

type Configs      = [Config]

class (Monad m) => MonadConfig m where
  configs :: m Configs

newtype ConfigurableT m a = ConfigurableT { run :: ReaderT Configs m a }
  deriving (Monad, Functor, Applicative, MonadReader Configs, MonadTrans)

configurable :: Configs -> ConfigurableT m a -> m a
configurable configs c = runReaderT (run c) configs

logWhenVerbose :: (MonadConfig m,MonadIO m) => String -> m ()
logWhenVerbose = when' Verbose . liftIO . hPutStrLn stderr

dumpAfterStage :: (MonadConfig m,MonadIO m) => String -> String -> m ()
dumpAfterStage stage content = do
  stageDump <- fromConfigs $ isStageDump stage
  case stageDump of
    Just filePath -> dump stage content filePath 
    Nothing       -> return ()

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

dump :: MonadIO m => String -> String -> FilePath -> m ()
dump title content filePath = liftIO $ case filePath of
  "" -> hPutStrLn stderr content'
  _  -> appendFile filePath $ content' ++ "\n"

  where content' = unwords [ "##", title
                           , replicate (50 - length title) '#', "\n"
                           , content]


{-
degree :: Configs -> Int
degree cs = case cs of
  (Degree d):_ -> d
  []           -> error "Compilation: No degree provided"
  _            -> degree $ tail cs

isDegreeLoop :: Configs -> Maybe Int
isDegreeLoop cs = case cs of
  (DegreeLoop d):_ -> Just d
  []               -> Nothing
  _                -> isDegreeLoop $ tail cs
-}

defaultInstantiationDepth = 10
instantiationDepth :: Configs -> Int
instantiationDepth cs = case cs of
  (InstantiationDepth d):_ -> d
  []                       -> defaultInstantiationDepth
  _                        -> instantiationDepth $ tail cs

isStageDump :: String -> Configs -> Maybe FilePath
isStageDump stage cs = case cs of
  (DumpAfter s fp):_ | s == stage -> Just fp
  (DumpAll         fp):_          -> Just fp
  []                              -> Nothing
  _                               -> isStageDump stage $ tail cs

undefinedSize :: Configs -> Maybe String
undefinedSize cs = case cs of
  (UndefinedSize s):_ -> Just s
  []                  -> Nothing
  _                   -> undefinedSize $ tail cs

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
  qLocation           = lift   qLocation
  qRunIO              = lift . qRunIO
  qAddDependentFile   = lift . qAddDependentFile

