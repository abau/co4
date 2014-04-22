{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Util
where

import           Control.Exception (assert)
import           Control.Monad.State
import           Data.List (nub)
import           Data.Maybe (mapMaybe)
import           Data.Either (partitionEithers)
import           Data.Tuple (swap)
import qualified Data.Map as M
import qualified TPDB.Data as TPDB
import qualified TPDB.Input as Input
import           CO4.PreludeNat (nat)
import           CO4.Util (bitWidth)
import           CO4.Test.TermComp2014.Standalone hiding (ord)

{-
import TPDB.Plain.Write ()
import TPDB.Pretty (pretty)
import Debug.Trace
-}

type SymbolMap = M.Map Symbol String

parseTrs :: FilePath -> IO (UnlabeledTrs, SymbolMap)
parseTrs path = Input.get_trs path >>= \ trs -> 
              do {-putStrLn (show $ pretty trs)-}

                 -- FIXME this is just for testing
                 -- Just trs <- return $ TPDB.Mirror.mirror trs 

                 return $ goTrs trs
  where
    goTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> (UnlabeledTrs, SymbolMap)
    goTrs trs = (Trs rules', M.fromList $ map swap $ M.toList symbolMap)
      where
        numIds              = numIdentifiers trs
        (rules', symbolMap) = runState (mapM goRule $ TPDB.rules trs) M.empty

        goRule rule = 
          return Rule `ap` (goTerm $ TPDB.lhs rule) `ap` (goTerm $ TPDB.rhs rule)

        goTerm (TPDB.Var v) = 
          return Var `ap` goIdentifier v

        goTerm (TPDB.Node v args) = 
          return Node `ap` goIdentifier v `ap` return () `ap` mapM goTerm args

        goIdentifier i = gets (M.lookup (TPDB.name i)) >>= \case
          Nothing -> do n <- gets M.size
                        let sym = assert (n < numIds)
                                $ nat (bitWidth numIds) $ fromIntegral n
                        modify $ M.insert (TPDB.name i) sym
                        return sym

          Just sym -> return sym

    numIdentifiers = length . nub . concatMap goRule . TPDB.rules
      where
        goRule rule = (goTerm $ TPDB.lhs rule) ++ (goTerm $ TPDB.rhs rule)
        goTerm term = (TPDB.lvars term) ++ (TPDB.lsyms term)

assignments :: Eq var => Int -> Trs var n l -> Assignments var
assignments n trs = do 
  values <- sequence $ replicate (length vars) [0..(2^n)-1]
  return $ zipWith goMapping vars values
  where
    vars                    = goTrs trs
    goTrs (Trs rules)       = nub $ concatMap goRule rules
    goRule (Rule l r)       = goTerm l ++ (goTerm r)
    goTerm (Var v)          = [v]
    goTerm (Node _  _ args) = concatMap goTerm args

    goMapping v value = (v, nat n value)

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
dependencyPairs (Trs rules) = Trs $ concatMap goRule rules
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
dpProblem trs = Trs $ original ++ dp
  where
    Trs original = trsToDp trs
    Trs dp       = dependencyPairs trs

trsToDp :: Trs Symbol Symbol label -> DPTrs label
trsToDp (Trs rules) = Trs $ map goRule rules
  where
    goRule (Rule lhs rhs)  = Rule (toDpTerm lhs) (toDpTerm rhs)

toDpTerm :: Term v n l -> Term v (n,Bool) l
toDpTerm (Var v)         = Var v
toDpTerm (Node s l args) = Node (s,False) l $ map toDpTerm args

ungroupTrs :: GroupedTrs v n l -> Trs v n l
ungroupTrs (GroupedTrs rules) = Trs $ concat rules

removeStrongDecreasingRules :: DPTrs () -> GroupedDPTrs Label -> [TerminationOrder MSL]
                            -> (DPTrs (), [DPRule ()])
removeStrongDecreasingRules (Trs rules) (GroupedTrs labeledRules) orders = 
    assert (length rules == length labeledRules) 
  $ (Trs keep, delete)
  where
    (delete, keep) = partitionEithers $ zipWith check rules labeledRules
  
    check rule labeledRules = 
      if all (isMarkedStrongDecreasingRule orders) labeledRules
      then Left  rule
      else Right rule

hasMarkedRule :: DPTrs label -> Bool
hasMarkedRule (Trs rules) = any goRule rules
  where
    goRule (Rule lhs _) = isMarked lhs
