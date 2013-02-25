{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Config 
where

import Control.Monad.Reader
import CO4.Unique (UniqueT)

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
             deriving (Eq,Show)

type Configs      = [Config]

class (Monad m) => MonadConfigurable m where
  configs :: m Configs

newtype ConfigurableT m a = ConfigurableT { run :: ReaderT Configs m a }
  deriving (Monad, MonadReader Configs, MonadTrans)

instance (Monad m) => MonadConfigurable (ConfigurableT m) where
  configs = ask

instance (MonadConfigurable m) => MonadConfigurable (UniqueT m) where
  configs = lift configs

instance (MonadIO m) => MonadIO (ConfigurableT m) where
  liftIO = lift . liftIO

configurable :: Configs -> ConfigurableT m a -> m a
configurable configs c = runReaderT (run c) configs

logWhenVerbose :: (MonadConfigurable m,MonadIO m) => String -> m ()
logWhenVerbose = when' Verbose . liftIO . putStrLn 

dumpAfterStage :: (MonadConfigurable m,MonadIO m) => String -> String -> m ()
dumpAfterStage stage content = do
  stageDump <- fromConfigs $ isStageDump stage
  case stageDump of
    Just filePath -> dump stage content filePath 
    Nothing       -> return ()

is :: (MonadConfigurable m,Monad m) => Config -> m Bool
is c = liftM (elem c) configs

when' :: (MonadConfigurable m) => Config -> m () -> m ()
when' c doThis = is c >>= \case True  -> doThis 
                                False -> return ()

whenNot' :: (MonadConfigurable m) => Config -> m () -> m ()
whenNot' c doThis = is c >>= \case False -> doThis 
                                   True  -> return ()

fromConfigs :: MonadConfigurable m => (Configs -> a) -> m a
fromConfigs f = configs >>= return . f

dump :: MonadIO m => String -> String -> FilePath -> m ()
dump title content filePath = liftIO $ case filePath of
  "" -> putStrLn content'
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
