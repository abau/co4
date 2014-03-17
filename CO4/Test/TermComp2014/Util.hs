{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Util
where

import           Control.Exception (assert)
import           Control.Monad (guard)
import           Data.Char (ord,isAsciiLower)
import           Data.List (nub)
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           Unsafe.Coerce
import qualified TPDB.Data as TPDB
import qualified TPDB.Plain.Read as Read
import           CO4.Util (toBinary, binaries)
import           CO4.Test.TermComp2014.Standalone 
  (Symbol,Assignments,Trs(..),Rule(..),Term(..),UnlabeledTrs)

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

dependecyPair :: (Eq v, Eq n, Eq l)  => Trs v n l -> Trs v (n, Bool) l
dependecyPair (Trs rules) = Trs $ concatMap goRule rules
  where
    defined = definedSymbols $ Trs rules

    goRule (Rule     (Var _)          _  ) = []
    goRule (Rule lhs@(Node ls ll lts) rhs) = do
      (Node us ul uts) <- us
      return $ Rule (Node (ls,True) ll $ map (markSymbols False) lts)
                    (Node (us,True) ul $ map (markSymbols False) uts)
      where
        us = do s@(Node f l _) <- filter (not . isVar) $ subterms rhs
                guard $ (f,l) `elem` defined
                guard $ not $ isSubterm s lhs
                return s

    markSymbols _ (Var v)       = Var v
    markSymbols m (Node s l ts) = Node (s,m) l $ map (markSymbols m) ts
