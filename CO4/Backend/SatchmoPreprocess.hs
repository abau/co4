{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Backend.SatchmoPreprocess
  (preprocessSatchmo)
where

import           Control.Monad.Identity
import           CO4.Language
import           CO4.Names 
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.TypedNames (eraseTypedNames)

newtype Preprocessor a = Preprocessor { runPP :: Identity a }
  deriving (Functor, Monad)

instance MonadInstantiator Preprocessor where

  instantiateVar (EVar (Name "&&"))       = return $ EVar $ Name "CO4.MonadifyTypes.and"
  instantiateVar (EVar (Name "||"))       = return $ EVar $ Name "CO4.MonadifyTypes.or"
  instantiateVar (EVar (Name "not"))      = return $ EVar $ Name "CO4.MonadifyTypes.not"
  instantiateVar exp = return exp

  instantiateCon (ECon c) | c == trueCon  = return $ EVar $ Name "CO4.MonadifyTypes.true"
  instantiateCon (ECon c) | c == falseCon = return $ EVar $ Name "CO4.MonadifyTypes.false"
  instantiateCon exp = return exp

preprocessSatchmo :: Instantiable a => a -> a
preprocessSatchmo = runIdentity . runPP . instantiate . eraseTypedNames
