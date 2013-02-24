{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.EtaExpansion
  (etaExpansion)
where

import           Control.Monad (forM)
import           CO4.Language
import           CO4.Unique
import           CO4.TypesUtil (argumentTypes,typeOfScheme)
import           CO4.Algorithms.Util (sanitize,eraseTypedNames)
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.HindleyMilner (schemes,schemeOfExp)

newtype Instantiator u a = Instantiator { runInstantiator :: u a }
  deriving (Monad, MonadUnique)

instance MonadUnique u => MonadInstantiator (Instantiator u) where
  
  instantiateLam (ELam ns e) = do
    e'     <- instantiate e
    params <- etaExpandExpression (ELam ns e) (length ns) 
    case params of
      [] -> return $ ELam ns e'
      ps -> return $ ELam (ns ++ ps) (EApp e' $ map EVar ps)

  instantiateBinding b@(Binding n e) = 
    case e of
      ELam {} -> instantiateLam e >>= return . Binding n
      _       -> do
        params <- etaExpandExpression e 0 
        case params of
          [] -> return b
          ps -> return $ Binding n $ ELam ps $ EApp e $ map EVar ps

  instantiateDeclaration d = return d

-- |Eta-expands an expression by counting the parameters of its type. 
-- Needs the number of already assigned parameters.
etaExpandExpression :: MonadUnique u => Expression -> Int -> u [Name]
etaExpandExpression exp numAssignedParameters = do
  scheme <- schemeOfExp exp

  let numParams = length (argumentTypes $ typeOfScheme scheme)

  if numParams > numAssignedParameters
    then forM [1 .. numParams - numAssignedParameters] $ const $ newName "eta"
    else return []

-- |Eta-expands bound expressions (@n = e => n = \x -> e x@) and 
-- lamda expressions (@\x -> e x => \x y -> e x y@).
etaExpansion :: (MonadUnique u) => Program -> u Program
etaExpansion program =
      schemes program 
  >>= runInstantiator . instantiate . sanitize
  >>= return . sanitize . eraseTypedNames
