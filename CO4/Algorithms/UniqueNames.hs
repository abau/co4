{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.UniqueNames
  (uniqueNames)
where

import           CO4.Language
import           CO4.Unique 
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.Rename (renameList)
import           CO4.Algorithms.Bound (bound)

newtype Instantiator u a = Instantiator { runInstantiator :: u a }
  deriving ( Functor, Monad, MonadUnique )

instance MonadUnique u => MonadInstantiator (Instantiator u) where
  instantiateLam (ELam names e) = do
    names' <- mapM newName names
    e'     <- instantiate $ renameList (zip names names') e 
    return $ ELam names' e' 

  instantiateLet (ELet n v e) = do
    n' <- newName n
    v' <- instantiate $ renameList [(n,n')] v
    e' <- instantiate $ renameList [(n,n')] e
    return $ ELet n' v' e'

  instantiateMatch (Match p e) =
    let names = bound p
    in do
      names' <- mapM newName names
      let p' =  renameList (zip names names') p
      e'     <- instantiate $ renameList (zip names names') e
      return $ Match p' e'

-- |Makes all bounded names unique 
uniqueNames :: (Instantiable a, MonadUnique u) => a -> u a
uniqueNames = runInstantiator . instantiate
  
