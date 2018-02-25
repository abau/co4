{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.SaturateApplication
  (saturateApplication)
where

import           Control.Monad.State.Strict
import           Control.Monad.Writer
import           Control.Applicative (Applicative)
import qualified Data.Map as M
import           CO4.Language
import           CO4.Unique
import           CO4.Names
import           CO4.Util (programDeclarations,programFromDeclarations)
import           CO4.TypesUtil (argumentTypes,typeOfScheme)
import           CO4.Algorithms.Util (sanitize,eraseTypedNames)
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.HindleyMilner (schemes,schemeOfExp)
import           CO4.Config (MonadConfig)

type CacheKey   = (Expression,Int) -- (Applied expression, number of passed arguments)
type CacheValue = Expression
type Cache      = M.Map CacheKey CacheValue

newtype Instantiator u a = Instantiator { 
    runInstantiator :: StateT Cache (WriterT [Declaration] u) a 
  }
  deriving ( Functor, Applicative, Monad, MonadUnique, MonadConfig, MonadWriter [Declaration]
           , MonadState Cache)

instance (MonadUnique u,MonadConfig u) => MonadInstantiator (Instantiator u) where
  instantiateApp (EApp f args) = do
    f'    <- instantiate f
    args' <- instantiate args

    scheme <- schemeOfExp f

    let numParams = length (argumentTypes $ typeOfScheme scheme)
        numArgs   = length args

    if numParams > numArgs 
      then do
        cache <- lookupCache (f',numArgs) 
        case cache of
          Just e  -> return $ EApp e args'
          Nothing -> newInstance f' args' numParams
      else return $ EApp f' args'

lookupCache :: MonadUnique u => CacheKey -> Instantiator u (Maybe CacheValue)
lookupCache key = gets $ M.lookup key

newInstance :: MonadUnique u => Expression -> [Expression] -> Int 
                             -> Instantiator u Expression
newInstance f args numParameters = do
  name <- case f of
            EVar n -> newName $ fromName (originalName n) ++ "Saturated"
            ECon n -> newName $ fromName (originalName n) ++ "Saturated"
            _      -> newName "saturated"

  availableParams <- forM [1 .. length args]                 $ const $ newName "sat"
  missingParams   <- forM [1 .. numParameters - length args] $ const $ newName "sat"

  let allArgs = map EVar $ availableParams ++ missingParams
      exp     = ELam availableParams $ ELam missingParams $ EApp f allArgs

  modify $ M.insert (f, length args) (EVar name)
  tell [ DBind $ Binding name exp ]

  return $ EApp (EVar name) args

-- |Saturates partial applications. Do not collapse abstractions afterwards as
-- this would infer partial applications again.
saturateApplication :: (MonadUnique u,MonadConfig u) => Program -> u Program
saturateApplication program = do
  typedProgram <- schemes program 
  (program',decls') <- runWriterT $ evalStateT 
                        (runInstantiator $ instantiate 
                                         $ sanitize
                                         $ programDeclarations typedProgram) 
                        M.empty
  return $ eraseTypedNames 
         $ programFromDeclarations  
         $ program' ++ decls'
