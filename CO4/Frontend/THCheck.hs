-- |Template Haskell compatibility checks
module CO4.Frontend.THCheck
  (check, checkProgram)
where

import           Language.Haskell.TH 
import           Data.Generics (GenericQ,everything,mkQ)
import           Data.List (partition)
import           Debug.Trace (trace)

data Message = Error String
             | Warning String

instance Show Message where
  show (Error msg)   = "Frontend.THCheck (Error): "   ++ msg
  show (Warning msg) = "Frontend.THCheck (Warning): " ++ msg

type Check   = [Message]

checkProgram :: [Dec] -> Bool
checkProgram = check 

check :: GenericQ Bool
check a = case partition isError (checks a) of
  ([],[]) -> True
  ([],ws) -> trace (unlines $ map show ws) $ True
  (es,ws) -> error $ unlines $ map show $ ws ++ es

checks :: GenericQ Check
checks a = concat  [ and' noGuardedBody a
                   , and' noBoundPatternsInValueDeclaration a
                   , and' validDeclaration a
                   , and' unresolvedInfixExp a
                   , and' unresolvedInfixPat a
                   ]

validDeclaration :: Dec -> Check
validDeclaration dec = case dec of
  FunD {}  -> []
  ValD {}  -> []
  DataD {} -> []
  _        -> [Error "Only function declarations, value declarations and data declarations are allowed"]

noGuardedBody :: Body -> Check
noGuardedBody (NormalB  _) = []
noGuardedBody (GuardedB _) = [Error "No guarded bodies allowed"]

noBoundPatternsInValueDeclaration :: Dec -> Check
noBoundPatternsInValueDeclaration dec = case dec of
  ValD (VarP _) _ _ -> []
  ValD p _ _        -> [Error $ concat ["Pattern '", show p, "' not allowed in ", excerpt dec]]
  _                 -> []

unresolvedInfixExp :: Exp -> Check
unresolvedInfixExp e = case e of 
  UInfixE {} -> [Warning $ "Unresolved infix expression '" ++ excerpt e ++ "'"]
  _          -> []

unresolvedInfixPat :: Pat -> Check
unresolvedInfixPat p = case p of 
  UInfixP {} -> [Warning $ "Unresolved infix pattern '" ++ excerpt p ++ "'"]
  _          -> []

and' f = everything (++) (mkQ [] f)
  
excerpt :: (Ppr a) => a -> String
excerpt a =  
  let noNewline '\n' = ' '
      noNewline c    = c
      prettyA        = map noNewline $ pprint a
      maxLength      = 70
  in
    if length prettyA > maxLength
    then take maxLength prettyA ++ "..."
    else prettyA

isError :: Message -> Bool
isError (Error _) = True
isError _         = False
