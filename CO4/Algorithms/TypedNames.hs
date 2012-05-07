{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Introduces @TypedName@s and erases them.
module CO4.Algorithms.TypedNames
  (typedNames,eraseTypedNames)
where

import           Control.Monad.Reader
import           Control.Monad.Identity
import           CO4.Names (typedName,untypedName)
import qualified CO4.Algorithms.HindleyMilner.Util as HM
import           CO4.Algorithms.Instantiator

newtype Instantiator a = Instantiator { runInstantiator :: Reader HM.Context a }
  deriving (Functor, Monad, MonadReader HM.Context)

newtype Eraser a = Eraser { runEraser :: Identity a }
  deriving (Functor, Monad)

instance MonadInstantiator Instantiator where
  instantiateName name = do
    context <- ask
    case HM.lookup name context of
      Nothing -> return name
      Just s  -> return $ typedName name s

instance MonadInstantiator Eraser where
  instantiateName = return . untypedName

typedNames :: Instantiable a => HM.Context -> a -> a
typedNames context a = runReader (runInstantiator $ instantiate a) context

eraseTypedNames :: Instantiable a => a -> a
eraseTypedNames = runIdentity . runEraser . instantiate  

