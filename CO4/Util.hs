{-# LANGUAGE Rank2Types #-}
-- |Utility functions
module CO4.Util
  ( everywhereM', topLevelNames, boundInProgram, boundName, boundExpression, rename, renames 
  , collapseFunApps)
where

import           Data.Generics (GenericM,GenericT,everywhere',everywhere,mkT,gmapM)
import           CO4.Language

-- | Monadic variation of 'Data.Generics.everywhere'', i.e. a monadic top-down transformation
everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x = f x >>= gmapM (everywhereM' f)

-- |Gets all names that are bound on the top level
topLevelNames :: Program -> [Name]
topLevelNames = map boundName

-- |Gets the declaration bound to a name
boundInProgram :: Name -> Program -> Maybe Declaration
boundInProgram name = boundInDeclarations
  where boundInDeclarations (d@(DBind n _) : ds) =
          if n == name then Just d
                       else boundInDeclarations ds
        boundInDeclarations [] = Nothing

-- |Gets the bound name of a declaration
boundName :: Declaration -> Name
boundName (DBind n _) = n

-- |Gets the bound expression of a declaration
boundExpression :: Declaration -> Expression
boundExpression (DBind _ e) = e

-- |List version of @rename@
renames :: [(Name,Name)] -> GenericT
renames = flip (foldr rename) 

-- |@rename (old,new)@ renames all occurences of @old@ to @new@ 
rename :: (Name,Name) -> GenericT
rename (old,new) = everywhere $ mkT (\name -> if name == old then new else name)

-- |Collapses function applications of the form @(f xs) ys@ to @f xs ys@
collapseFunApps :: GenericT
collapseFunApps = everywhere' (mkT collapseExpression)
  where 
    collapseExpression (EApp (EApp f xs) ys)      = EApp f $ xs ++ ys
    collapseExpression exp                        = exp
