{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.Bound
  (boundInPattern, boundInScheme, boundToplevel)
where

import           Data.List (nub)
import           CO4.Language
import           CO4.Util (programToplevelBindings)
import           CO4.Names (name)

-- |Gets names that are bound in a pattern
boundInPattern :: Pattern -> [Name]
boundInPattern (PVar n)    = [n]
boundInPattern (PCon _ ps) = nub $ concatMap boundInPattern ps

-- |Gets names that are bound in a scheme
boundInScheme :: Scheme -> [Name]
boundInScheme (SType _)     = []
boundInScheme (SForall n s) = (name n) : boundInScheme s

-- |Gets names that are bound in top-level bindings
boundToplevel :: Program -> [Name]
boundToplevel = map boundName . programToplevelBindings
