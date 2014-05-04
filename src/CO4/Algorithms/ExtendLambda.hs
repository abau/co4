{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.ExtendLambda
  (extendLambda)
where

import           Control.Monad (forM)
import           Control.Applicative (Applicative)
import           CO4.Language
import           CO4.Unique
import           CO4.TypesUtil (argumentTypes,typeOfScheme)
import           CO4.Algorithms.Util (sanitize,eraseTypedNames)
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.HindleyMilner (schemes,schemeOfExp)
import           CO4.Config (MonadConfig)

-- |Extends lamda expressions (@\x -> e x => \x y -> e x y@) and
-- eta-expands bound expressions (@n = e => n = \x -> e x@).
extendLambda :: (MonadUnique u,MonadConfig u) => Program -> u Program
extendLambda program =
      schemes program 
  >>= runInstantiator . instantiate . sanitize
  >>= return . sanitize . eraseTypedNames

newtype Instantiator u a = Instantiator { runInstantiator :: u a }
  deriving (Functor, Applicative, Monad, MonadUnique, MonadConfig)

instance (MonadUnique u,MonadConfig u) => MonadInstantiator (Instantiator u) where
  
  instantiateLam (ELam ns e) = do
    e'     <- instantiate e
    params <- expandExpression (ELam ns e') (length ns) 
    case params of
      [] -> return $ ELam ns e'
      ps -> return $ ELam (ns ++ ps) (EApp e' $ map EVar ps)

  instantiateBinding (Binding n e@(ELam {})) = instantiateLam e >>= return . Binding n
  instantiateBinding (Binding n e)           = do
    e'     <- instantiate e
    params <- expandExpression e' 0 
    case params of
      [] -> return $ Binding n e'
      ps -> return $ Binding n $ ELam ps $ EApp e' $ map EVar ps

-- |@expandExpression e n@ expands @e@ by counting the parameters of its type. 
-- @n@ is the number of already assigned parameters.
expandExpression :: (MonadUnique u,MonadConfig u) => Expression -> Int -> u [Name]
expandExpression exp numAssignedParameters = do
  scheme <- schemeOfExp exp

  let numParams = length (argumentTypes $ typeOfScheme scheme)

  if numParams > numAssignedParameters
    then forM [1 .. numParams - numAssignedParameters] $ const $ newName "eta"
    else return []
