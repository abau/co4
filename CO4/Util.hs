{-# LANGUAGE Rank2Types #-}
-- |Utility functions
module CO4.Util
  (topLevelNames, declarationByName, splitDeclarations, dAdt)
where

import           Data.Maybe (mapMaybe)
import           CO4.Language

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

-- |Returns the algebraic data type in terms of a @Type@ instance
dAdt :: Declaration -> Type
dAdt adt = TCon (dAdtName adt) $ map TVar $ dAdtTypeVariables adt
