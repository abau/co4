{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Globalize
  (globalize)
where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader 
import           Control.Monad.Writer
import           Data.List (find,(\\))
import           Data.Maybe (fromMaybe)
import           CO4.Language
import qualified CO4.Util as U
import           CO4.Util (renames)
import           CO4.Unique (Unique,newName)
import           CO4.Algorithms.HindleyMilner (freeInPrelude)
import           CO4.Algorithms.Instantiator

data Env = Env { bindings      :: [Declaration]
               , topLevelNames :: [Name]
               }

newtype Instantiator a = Instantiator (ReaderT Env (WriterT [Declaration] Unique) a)
  deriving (Functor, Monad, MonadReader Env, MonadWriter [Declaration])

instance MonadInstantiator Instantiator where

  instantiateVar (EVar name) = do
    bs <- asks bindings
    return $ fromMaybe (EVar name) 
           $ fmap dBindExpression 
           $ find (\d -> dBindName d == name) bs

  instantiateLam exp = 
    newGlobalName >>= \name -> instantiateLambdaWithName name exp

  -- This rule is not neccessary for globalization itself, but it's a convenient
  -- shortcut and it prevents currying: otherwise @let n = \foo -> bar@ would
  -- result in two declarations @global = \foo -> bar@ and @n = global@.
  instantiateLet (ELet name v@(ELam {}) e) = do
    v' <- instantiateLambdaWithName name v
    local (addBinding name v') $ instantiateExpression e

  instantiateLet (ELet name v e) = do
    v' <- instantiateExpression v
    e' <- instantiateExpression e
    return $ ELet name v' e'
    
newGlobalName :: Instantiator Name
newGlobalName = liftUnique $ newName "global" 

liftUnique :: Unique a -> Instantiator a
liftUnique = Instantiator . lift . lift

instantiateLambdaWithName :: Name -> Expression -> Instantiator Expression
instantiateLambdaWithName name (ELam parameters e) = do
  nonFree     <- (name :) <$> asks topLevelNames 
  let free    =  freeInPrelude (ELam parameters e) \\ nonFree
      result  =  if null free then EVar name 
                              else EApp (EVar name) $ map EVar free

  local (addBinding name result) $ do
    e'          <- instantiateExpression e
    free'       <- liftUnique $ mapM newName free
    let e''     =  renames (zip free free') e'
        newDec  =  case free' ++ parameters of
                    [] -> DBind name e''
                    p  -> DBind name $ ELam p e''
    tell [newDec]
    return result

-- |Lifts all local function declarations to the top level. 
-- Make sure that all bounded names are unique and all expressions uncurried.
globalize :: Program -> Unique Program
globalize program = concat <$> mapM globalizeTopLevel program
  where 
    globalizeTopLevel declaration = case declaration of
      DBind n (ELam ns e) -> do
        (e',decs) <- runInstantiator (instantiateExpression e) tlNames
        return $ (DBind n $ ELam ns e') : decs 
      DBind n e -> do
        (e',decs) <- runInstantiator (instantiateExpression e) tlNames
        return $ (DBind n e') : decs 

    tlNames = U.topLevelNames program

runInstantiator :: Instantiator a -> [Name] -> Unique (a,[Declaration])
runInstantiator (Instantiator inst) tlNames =
  runWriterT $ runReaderT inst (Env [] tlNames)

addBinding :: Name -> Expression -> Env -> Env
addBinding n e env = 
  env { bindings = (DBind n e) : (bindings env) }
