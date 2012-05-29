-- |Template Haskell compatibility checks
module CO4.Frontend.THCheck
  (check)
where

import           Language.Haskell.TH 
import           Data.Generics (GenericQ,everything,mkQ)
import           Data.List (partition)
import           Debug.Trace (trace)

data Message = Error String
             | Warning String
             deriving (Show)
type Check   = [Message]

check :: Monad m => GenericQ (m ())
check a = case partition isError (checkMsg a) of
  ([],[]) -> return ()
  ([],ws) -> trace (unlines $ map show ws) $ return ()
  (es,ws) -> error $ concat $ (map show ws) ++ (map show es)

checkMsg :: GenericQ Check
checkMsg a = concat  [ and' noGuardedBody a
                     , and' noBoundPatternsInValueDeclaration a
                     , and' validDeclaration a
                     , and' unresolvedInfixExp a
                     , and' unresolvedInfixPat a
                     ]

validDeclaration :: Dec -> Check
validDeclaration dec = case dec of
  FunD {}  -> []
  ValD {}  -> []
  SigD {}  -> []
  DataD {} -> []
  _        -> [Error "Only function declarations, value declarations, signatures and data declarations are allowed"]

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
