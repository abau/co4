-- |Template Haskell compatibility checks
module CO4.Frontend.THCheck
  (check)
where

import           Language.Haskell.TH 
import           Data.Generics (GenericQ,everything,mkQ)

type Message = String
type Check   = [Message]

check :: GenericQ Check
check a = concat  [ and' noGuardedBody a
                  , and' noBoundPatternsInValueDeclaration a
                  , and' validDeclaration a
                  ]

validDeclaration :: Dec -> Check
validDeclaration dec = case dec of
  FunD {} -> []
  ValD {} -> []
  SigD {} -> []
  _       -> ["Only function declarations, value declarations and signatures are allowed"]

noGuardedBody :: Body -> Check
noGuardedBody (NormalB  _) = []
noGuardedBody (GuardedB _) = ["No guarded bodies allowed"]

noBoundPatternsInValueDeclaration :: Dec -> Check
noBoundPatternsInValueDeclaration dec = case dec of
  ValD (VarP _) _ _ -> []
  ValD p _ _        -> [unwords ["Pattern", show p, "not allowed in", excerpt dec]]
  _                 -> []

and' f = everything (++) (mkQ [] f)
  
excerpt :: (Ppr a) => a -> String
excerpt a = take 70 (takeWhile (/= '\n') (pprint a)) ++ "..."
