{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.EtaExpansion
  (etaExpansion)
where

import           Control.Monad (forM)
import           Data.Data (Data)
import           CO4.Language
import           CO4.Unique
import           CO4.TypesUtil (argumentTypes,typeOfScheme)
import           CO4.Util (collapseLam,collapseApp)
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.HindleyMilner (schemeOfExp, prelude)

newtype Instantiator u a = Instantiator { runInstantiator :: u a }
  deriving ( Functor, Monad, MonadUnique )

instance MonadUnique u => MonadInstantiator (Instantiator u) where
  
  instantiateApp (EApp f args) = do
    args'   <- instantiate args
    params  <- etaExpandExpression (EApp f args) 0 
    case params of
      [] -> return $ EApp f args'
      ps -> return $ ELam ps $ EApp f $ args' ++ (map EVar ps)

  instantiateLam (ELam ns e) = do
    e'     <- instantiate e
    params <- etaExpandExpression (ELam ns e) (length ns) 
    case params of
      [] -> return $ ELam ns e'
      ps -> return $ ELam (ns ++ ps) (EApp e' $ map EVar ps)

  instantiateDeclaration d@(DBind n e@(EVar _)) = do
    params <- etaExpandExpression e 0 
    case params of
      [] -> return d
      ps -> return $ DBind n $ ELam ps $ EApp e $ map EVar ps

  instantiateDeclaration (DBind n e) = instantiate e >>= return . DBind n

etaExpandExpression :: MonadUnique u => Expression -> Int -> u [Name]
etaExpandExpression exp numAssignedParameters = do
  scheme <- schemeOfExp prelude exp

  let numParams = length (argumentTypes $ typeOfScheme scheme)

  if numParams > numAssignedParameters
    then forM [1 .. numParams - numAssignedParameters] $ const $ newName' "eta"
    else return []

-- TODO: Data
etaExpansion :: (MonadUnique u, Instantiable i, Data i) => i -> u i
etaExpansion i = do
  i' <- runInstantiator $ instantiate $ collapseLam $ collapseApp i
  return $ collapseLam $ collapseApp i'
