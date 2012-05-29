{-# LANGUAGE Rank2Types #-}
-- |Utility functions
module CO4.Util
  ( everywhereM', topLevelNames, declarationByName, splitDeclarations, rename, renames 
  , collapseFunApps, dAdt)
where

import           Data.Generics (GenericM,GenericT,everywhere',everywhere,mkT,gmapM)
import           Data.Maybe (mapMaybe)
import           CO4.Language

-- | Monadic variation of 'Data.Generics.everywhere'', i.e. a monadic top-down transformation
everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x = f x >>= gmapM (everywhereM' f)

-- |Gets all names that are bound on the top level
topLevelNames :: Program -> [Name]
topLevelNames = mapMaybe topLevelName
  where topLevelName (DBind n _) = Just n
        topLevelName _           = Nothing

-- |Gets the declaration bound to a name
declarationByName :: Name -> Program -> Maybe Declaration
declarationByName name = boundInDeclarations
  where boundInDeclarations (d@(DBind n _) : ds) =
          if n == name then Just d
                       else boundInDeclarations ds
        boundInDeclarations [] = Nothing

-- |Splits declarations into type declarations and value declaration
splitDeclarations :: Program -> ([Declaration], [Declaration])
splitDeclarations = foldl split ([],[])
  where split (types, vals) d@(DAdt {}) = (types ++ [d], vals)
        split (types, vals) d           = (types, vals ++ [d])

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

-- |Returns the algebraic data type in terms of a @Type@ instance
dAdt :: Declaration -> Type
dAdt adt = TCon (dAdtName adt) $ map TVar $ dAdtTypeVariables adt
