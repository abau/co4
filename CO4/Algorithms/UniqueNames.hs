{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.UniqueNames
  (uniqueLocalNames, uniqueNames)
where

import           Control.Monad (forM)
import           CO4.Language
import           CO4.Unique 
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.Rename (renameList)
import           CO4.Algorithms.Bound (boundInPattern,boundToplevel)

newtype Instantiator u a = Instantiator { runInstantiator :: u a }
  deriving (Monad, MonadUnique)

instance MonadUnique u => MonadInstantiator (Instantiator u) where
  instantiateLam (ELam names e) = do
    names' <- mapM newName names
    e'     <- instantiate $ renameList (zip names names') e 
    return $ ELam names' e' 

  instantiateLet (ELet bindings e) = do
    let boundNames = map boundName bindings
    newNames <- forM boundNames newName

    let renamings = zip boundNames newNames

    bindings' <- forM bindings $ instantiate . renameList renamings
    e'        <-                 instantiate $ renameList renamings e
    return $ ELet bindings' e'

  instantiateMatch (Match p e) =
    let names = boundInPattern p
    in do
      names' <- mapM newName names
      let p' =  renameList (zip names names') p
      e'     <- instantiate $ renameList (zip names names') e
      return $ Match p' e'

-- |Makes all locally bounded names unique 
uniqueLocalNames :: (Instantiable a, MonadUnique u) => a -> u a
uniqueLocalNames = runInstantiator . instantiate
  
-- |Makes all bounded names unique, also top level names
uniqueNames :: MonadUnique u => Program -> u Program
uniqueNames program = do
  let ns =  boundToplevel program
  ns'    <- mapM newName ns
  uniqueLocalNames $ renameList (zip ns ns') program
