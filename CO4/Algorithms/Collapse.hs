{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Collapse
  (collapseApp, collapseLam)
where

import           Control.Monad.Identity
import           CO4.Language
import           CO4.Algorithms.Instantiator

newtype AppInstantiator a = AppInstantiator { runAppInstantiator :: Identity a }
  deriving ( Functor, Monad )

newtype LamInstantiator a = LamInstantiator { runLamInstantiator :: Identity a }
  deriving ( Functor, Monad )

instance MonadInstantiator AppInstantiator where
  instantiateApp (EApp (EApp f xs) ys) = instantiate $ EApp f $ xs ++ ys

  instantiateApp (EApp f xs) = do
    f'  <- instantiate f
    xs' <- instantiate xs
    return $ EApp f' xs'

instance MonadInstantiator LamInstantiator where
  instantiateLam (ELam xs (ELam ys e)) = instantiate $ ELam (xs ++ ys) e

  instantiateLam (ELam ns e) = do
    e'  <- instantiate e
    return $ ELam ns e'

-- |Collapses function applications of the form @(f xs) ys@ to @f xs ys@
collapseApp :: Instantiable a => a -> a
collapseApp = runIdentity . runAppInstantiator . instantiate

-- |Collapses abstractions of the form @\x -> \y -> e@ to @\x y -> e@
collapseLam :: Instantiable a => a -> a
collapseLam = runIdentity . runLamInstantiator . instantiate
