-- |Template Haskell compatibility checks
module CO4.Frontend.THCheck
  (check)
where

import           Language.Haskell.TH 
import           Data.Generics (GenericQ,everything,mkQ)
import           Data.List (partition,nub)
import           Debug.Trace (trace)

data Message = Error String
             | Warning String
             deriving (Eq)

instance Show Message where
  show (Error msg)   = "Frontend.THCheck (Error): "   ++ msg
  show (Warning msg) = "Frontend.THCheck (Warning): " ++ msg

type Check   = [Message]

check :: GenericQ Bool
check a = case partition isError (checks a) of
  ([],[]) -> True
  ([],ws) -> trace (unlines $ map show $ nub ws) $ True
  (es,ws) -> error $ unlines $ map show $ nub $ ws ++ es

checks :: GenericQ Check
checks a = concat  [ and' noGuardedBody a
                   , and' noBoundPatternsInValueDeclaration a
                   , and' validDeclaration a
                   , and' unresolvedInfixExp a
                   , and' unresolvedInfixPat a
                   , and' noOverlappingDefaultMatches  a
                   ]

validDeclaration :: Dec -> Check
validDeclaration dec = case dec of
  FunD {}          -> []
  ValD {}          -> []
  DataD _ _ _ _ [] -> []
  DataD _ _ _ _ _  -> [Warning "Deriving statements will be ignored"]
  TySynD {}        -> []
  SigD {}          -> [Warning "Type signatures will be deleted"]
  _                -> [Error "Only function declarations, value declarations and data declarations are allowed"]

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

noOverlappingDefaultMatches :: Exp -> Check
noOverlappingDefaultMatches exp = case exp of
  CaseE _ ms -> if ( length (filter isDefaultMatch ms) > 1  )
                || ( any isDefaultMatch $ tail $ reverse ms )
                then [Error $ "Overlapping default matches in '" ++ excerpt exp ++ "'"]
                else []
  _ -> []
  where
    isDefaultMatch (Match (VarP  _) _ _) = True
    isDefaultMatch (Match WildP     _ _) = True
    isDefaultMatch _                     = False

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
