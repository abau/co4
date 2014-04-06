{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Globalize
  (globalize)
where

import           Control.Monad.State.Strict
import           Data.List (partition,(\\),nub)
import qualified Data.Map as M
import           CO4.Language
import           CO4.Util (addDeclarations)
import           CO4.Unique (MonadUnique,newName)
import           CO4.Algorithms.Free (Free,free)
import           CO4.Algorithms.Bound (boundToplevel)
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.Rename (renameList)
import           CO4.Algorithms.Replace (replaceExpressions)
import           CO4.Algorithms.Util (collapseApplications)
import           CO4.Algorithms.TopologicalSort (bindingGroups)
import           CO4.Prelude (unparsedNames)

data Env = Env { callGlobal       :: M.Map Name Expression 
               , globalBindings   :: [Binding]
               , originalToplevel :: [Name]
               }

newtype Globalizer u a = Globalizer { runGlobalizer :: StateT Env u a }
  deriving (Monad, MonadState Env, MonadUnique)

instance MonadUnique u => MonadInstantiator (Globalizer u) where

  instantiateVar (EVar name) = liftM (M.findWithDefault (EVar name) name) $ gets callGlobal

  instantiateLam (ELam parameters e) = do
    e'         <- instantiateExpression  e
    globalName <- newName "globalLambda"
    freeNames  <- getFreeNames $ ELam parameters e'
    freeNames' <- forM freeNames newName

    addGlobalBinding $ Binding globalName 
                     $ ELam (freeNames' ++ parameters) 
                     $ renameList (zip freeNames freeNames') e'

    case freeNames of
      [] -> return $       EVar globalName
      _  -> return $ EApp (EVar globalName) $ map EVar freeNames

  instantiateLet (ELet bindings e) = 
    let (funBindings,otherBindings) = partition isFunctionBinding bindings
        funBindingGroups            = bindingGroups funBindings
    in do
      oldCalls <- gets callGlobal

      forM_ funBindingGroups globalizeFunBindingGroup

      otherBindings' <- forM otherBindings instantiateBinding

      result <- case otherBindings' of
                  [] -> instantiate e
                  _  -> liftM (ELet otherBindings') $ instantiate e

      modify $ \env -> env { callGlobal = oldCalls }
      return result

    where 
      isFunctionBinding (Binding _ (ELam {})) = True
      isFunctionBinding _                     = False

      globalizeFunBindingGroup bGroup = do
        let boundNames  = map boundName bGroup
            boundExps   = map boundExpression bGroup

        boundExps' <- forM boundExps $ \(ELam p exp) -> liftM (ELam p) $ instantiate exp

        freeNames <- do all <- forM boundExps' getFreeNames 
                        return $ nub (concat all) \\ boundNames

        let globalCall name = EApp (EVar name) $ map EVar freeNames

        forM_ boundNames $ \n -> addGlobalCall n $ globalCall n

        let replacements = map (\n -> (EVar n, globalCall n) ) boundNames 

        forM_ (zip boundNames boundExps') $ \(name, ELam params exp) -> do
          freeNames' <- forM freeNames newName
          let exp' = renameList (zip freeNames freeNames')
                        $ replaceExpressions replacements 
                        $ exp
          addGlobalBinding $ Binding name $ ELam (freeNames' ++ params) exp'

  instantiateBind (DBind binding) = case binding of
      Binding n (ELam ns e) -> liftM (DBind . Binding n . ELam ns) $ instantiate e
      Binding n e           -> liftM (DBind . Binding n          ) $ instantiate e

getFreeNames :: (Free a,Monad m) => a -> Globalizer m [Name]
getFreeNames a = do
  globalNames   <- gets (map boundName . globalBindings)
  toplevelNames <- gets originalToplevel
  return $ free a \\ (globalNames ++ toplevelNames)

addGlobalCall :: Monad m => Name -> Expression -> Globalizer m ()
addGlobalCall name exp = 
  modify ( \state -> state { callGlobal = M.insert name exp $ callGlobal state } )

addGlobalBinding :: Monad m => Binding -> Globalizer m ()
addGlobalBinding b = 
  modify ( \state -> state { globalBindings = globalBindings state ++ [b] } )

-- |Lifts all local function declarations to the top level. 
-- Make sure that all bounded names are unique and all expressions uncurried.
globalize :: MonadUnique u => Program -> u Program
globalize program = do
  (program',state) <- runStateT ( runGlobalizer $ instantiate program ) 
                                $ Env M.empty []
                                $ boundToplevel program ++ unparsedNames
  return $ collapseApplications 
         $ addDeclarations (map DBind $ globalBindings state) program' 
