{-# LANGUAGE Rank2Types #-}
module CO4.Algorithms.UniqueNames
  (uniqueNames)
where

import           Data.Generics (GenericM,mkM,everywhereM)
import           CO4.Language
import           CO4.Unique (Unique,newName)
import           CO4.Util (rename,renames)
import           CO4.Algorithms.Bound (bound)

-- |Makes all bounded names unique 
uniqueNames :: GenericM Unique
uniqueNames a = everywhereM (mkM uniqueNamesInExpression) a 
            >>= everywhereM (mkM uniqueNamesInMatch)

uniqueNamesInExpression :: Expression -> Unique Expression
uniqueNamesInExpression expression = case expression of
  ELam names e -> do
    names' <- mapM newName names
    return $ ELam names' $ renames (zip names names') e

  ELet n v e -> do
    n' <- newName n
    return $ ELet n' (rename (n,n') v) (rename (n,n') e)

  _ -> return expression
  
uniqueNamesInMatch :: Match -> Unique Match
uniqueNamesInMatch (Match p e) = 
  let names = bound p
  in do
    names' <- mapM newName names
    let renamings = zip names names'
    return $ Match (renames renamings p) $ renames renamings e
