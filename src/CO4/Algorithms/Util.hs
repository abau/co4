{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Provides various functions that operate on the syntax tree in a simple manner.
module CO4.Algorithms.Util
  ( introduceTypedNames, eraseTypedNames
  , collapseApplications, collapseAbstractions, sanitize
  , eraseTLamTApp
  )
where

import           Control.Monad.Reader
import           Control.Monad.Identity
import           CO4.Language
import           CO4.Names (untypedName)
import           CO4.Util (nTyped,nUntyped)
import qualified CO4.Algorithms.HindleyMilner.Util as HM
import           CO4.Algorithms.Instantiator

newtype IntroTypedNames a = IntroTypedNames { introTypedNames :: Reader HM.Context a }
  deriving (Monad, MonadReader HM.Context)

instance MonadInstantiator IntroTypedNames where
  instantiateName name = do
    context <- ask
    case HM.lookup (untypedName name) context of
      Nothing -> return name
      Just s  -> return $ nTyped name s

-- |Introduces typed names
introduceTypedNames :: Instantiable a => HM.Context -> a -> a
introduceTypedNames context a = runReader (introTypedNames $ instantiate a) context

newtype EraseTypedNames a = EraseTypedNames { eraseTypedNames_ :: Identity a }
  deriving (Monad)

instance MonadInstantiator EraseTypedNames where
  instantiateName = return . nUntyped

-- |Erases typed names
eraseTypedNames :: Instantiable a => a -> a
eraseTypedNames = runIdentity . eraseTypedNames_ . instantiate  

newtype CollapseApps a = CollapseApps { collapseApps :: Identity a }
  deriving (Monad)

instance MonadInstantiator CollapseApps where
  instantiateApp (EApp (EApp f xs) ys) = instantiate $ EApp f $ xs ++ ys

  instantiateApp (EApp f xs) = return EApp `ap` instantiate f `ap` instantiate xs
  
-- |Collapses function applications of the form @(f xs) ys@ to @f xs ys@
collapseApplications :: Instantiable a => a -> a
collapseApplications = runIdentity . collapseApps . instantiate

newtype CollapseAbs a = CollapseAbs { collapseAbs :: Identity a }
  deriving (Monad)

instance MonadInstantiator CollapseAbs where
  instantiateLam (ELam xs (ELam ys e)) = instantiate $ ELam (xs ++ ys) e

  instantiateLam (ELam ns e) = return (ELam ns) `ap` instantiate e

-- |Collapses abstractions of the form @\x -> \y -> e@ to @\x y -> e@
collapseAbstractions :: Instantiable a => a -> a
collapseAbstractions = runIdentity . collapseAbs . instantiate

-- | @collapseApplications@ and @collapseAbstractions@ 
sanitize :: Instantiable a => a -> a
sanitize = collapseApplications . collapseAbstractions 

newtype EraseTLamTApp a = EraseTLamTApp { eraseTLamTApp_ :: Identity a }
  deriving (Monad)

instance MonadInstantiator EraseTLamTApp where
  instantiateTApp (ETApp e _) = return e
  instantiateTLam (ETLam _ e) = return e

-- |Erases type abstractions and type applications
eraseTLamTApp :: Instantiable a => a -> a
eraseTLamTApp = runIdentity . eraseTLamTApp_ . instantiate
