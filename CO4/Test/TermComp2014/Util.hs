{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Util
where

import           Control.Exception (assert)
import           Control.Monad (guard)
import           Data.Char (ord,isAsciiLower)
import           Data.List (nub)
import           Data.Maybe (mapMaybe)
import           Data.Either (partitionEithers)
import qualified Data.Map as M
import qualified TPDB.Data as TPDB
import qualified TPDB.Plain.Read as Read
import           CO4.Util (toBinary, binaries)
import           CO4.Test.TermComp2014.Standalone hiding (ord)

{-
import TPDB.Plain.Write ()
import TPDB.Pretty (pretty)
import Debug.Trace
-}

parseTrs :: FilePath -> IO UnlabeledTrs
parseTrs path = readFile path >>= return . Read.trs >>= \case
  Left msg  ->    error msg
  Right trs -> do {-putStrLn (show $ pretty trs)-}
                  return $ goTrs trs
  where
    goTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> UnlabeledTrs
    goTrs                     = Trs . map goRule . TPDB.rules
    goRule rule               = Rule (goTerm $ TPDB.lhs rule) (goTerm $ TPDB.rhs rule)
    goTerm (TPDB.Var v)       = Var $ goIdentifier v
    goTerm (TPDB.Node v args) = Node (goIdentifier v) () $ map goTerm args

    goIdentifier i | isValidIdentifier i = charToSymbol $ head $ TPDB.name i
    goIdentifier i                       = error $ "Invalid identifier '" ++ TPDB.name i ++ "'"

    isValidIdentifier i = (length (TPDB.name i) == 1) && (isAsciiLower $ head $ TPDB.name i) 
    charToSymbol        = toBinary Nothing . ord

assignments :: Eq var => Int -> Trs var n l -> Assignments var
assignments n trs = do 
  values <- sequence $ replicate (length vars) $ binaries n
  return $ zipWith goMapping vars values
  where
    vars                    = goTrs trs
    goTrs (Trs rules)       = nub $ concatMap goRule rules
    goRule (Rule l r)       = goTerm l ++ (goTerm r)
    goTerm (Var v)          = [v]
    goTerm (Node _  _ args) = concatMap goTerm args

    goMapping v value = (v, value)

nodeArities :: Ord node => Trs v node l -> M.Map node Int
nodeArities (Trs rules) = M.fromListWith (\a b -> assert (a == b) a) 
                        $ concatMap goRule rules
  where 
    goRule (Rule l r)      = (goTerm l) ++ (goTerm r)
    goTerm (Var {})        = []
    goTerm (Node v _ args) = (v, length args) : (concatMap goTerm args)

isVar :: Term n v l -> Bool
isVar (Var _) = True
isVar _       = False

isSubterm :: (Eq v, Eq n, Eq l) => Term v n l -> Term v n l -> Bool
isSubterm subterm = go
  where
    go t@(Var _)       = t == subterm
    go t@(Node _ _ ts) = (t == subterm) || (any go ts)

subterms :: Term v n l -> [Term v n l]
subterms = go
  where
    go t@(Var _)       = [t]
    go t@(Node _ _ ts) = t : (concatMap go ts)

definedSymbols :: Trs v node label -> [(node, label)]
definedSymbols (Trs rules) = mapMaybe goRule rules
  where
    goRule (Rule lhs _) = goTerm lhs
    goTerm (Var _)      = Nothing
    goTerm (Node s l _) = Just (s,l)

dependencyPairs :: UnlabeledTrs -> DPTrs ()
dependencyPairs (Trs rules) = DPTrs $ do r <- concatMap goRule rules
                                         return [r]
  where
    defined = definedSymbols $ Trs rules

    goRule (Rule     (Var _)          _  ) = []
    goRule (Rule lhs@(Node ls ll lts) rhs) = do
      (Node us ul uts) <- us
      return $ Rule (Node (ls,True) ll $ map toDpTerm lts)
                    (Node (us,True) ul $ map toDpTerm uts)
      where
        us = do s@(Node f l _) <- filter (not . isVar) $ subterms rhs
                guard $ (f,l) `elem` defined
                guard $ not $ isSubterm s lhs
                return s

dpProblem :: UnlabeledTrs -> DPTrs ()
dpProblem trs = DPTrs $ original ++ dp
  where
    DPTrs original = trsToDp trs
    DPTrs dp       = dependencyPairs trs

dpToTrs :: DPTrs label -> Trs Symbol MarkedSymbol label
dpToTrs (DPTrs rules) = Trs $ concat rules

trsToDp :: Trs Symbol Symbol label -> DPTrs label
trsToDp (Trs rules) = DPTrs $ map (return . goRule) rules
  where
    goRule (Rule lhs rhs)  = Rule (toDpTerm lhs) (toDpTerm rhs)

toDpTerm :: Term v n l -> Term v (n,Bool) l
toDpTerm (Var v)         = Var v
toDpTerm (Node s l args) = Node (s,False) l $ map toDpTerm args

removeStrongDecreasingRules :: DPTrs () -> DPTrs Label -> [Precedence MarkedSymbol Label]
                            -> (DPTrs (), [DPRule ()])
removeStrongDecreasingRules (DPTrs rules) (DPTrs labeledRules) precedences = 
    assert (length rules == length labeledRules) 
  $ (DPTrs $ map return keep, delete)
  where
    (delete, keep) = partitionEithers $ zipWith check rules labeledRules
  
    check [rule] labeledRules = 
      if all (isMarkedStrongDecreasingRule precedences) labeledRules
      then Left  rule
      else Right rule

hasMarkedRule :: DPTrs label -> Bool
hasMarkedRule (DPTrs rules) = any (any goRule) rules
  where
    goRule (Rule lhs _) = isMarked lhs
