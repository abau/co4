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
  (Symbol,Assignments,Trs(..),Rule(..),Term(..))

{-
import TPDB.Plain.Write ()
import TPDB.Pretty (pretty)
import Debug.Trace
-}

parseTrs :: FilePath -> IO (Trs ())
parseTrs path = readFile path >>= return . Read.trs >>= \case
  Left msg  ->    error msg
  Right trs -> do {-putStrLn (show $ pretty trs)-}
                  return $ goTrs trs
  where
    goTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> Trs ()
    goTrs                     = Trs . map goRule . TPDB.rules
    goRule rule               = Rule (goTerm $ TPDB.lhs rule) (goTerm $ TPDB.rhs rule)
    goTerm (TPDB.Var v)       = Var $ goIdentifier v
    goTerm (TPDB.Node v args) = Node (goIdentifier v) () $ map goTerm args

    goIdentifier i | isValidIdentifier i = charToSymbol $ head $ TPDB.name i
    goIdentifier i                       = error $ "Invalid identifier '" ++ TPDB.name i ++ "'"

    isValidIdentifier i = (length (TPDB.name i) == 1) && (isAsciiLower $ head $ TPDB.name i) 
    charToSymbol        = toBinary Nothing . ord

assignments :: Int -> Trs a -> Assignments
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

nodeArities :: Trs a -> M.Map Symbol Int
nodeArities (Trs rules) = M.fromListWith (\a b -> assert (a == b) a) 
                        $ concatMap goRule rules
  where 
    goRule (Rule l r)      = (goTerm l) ++ (goTerm r)
    goTerm (Var {})        = []
    goTerm (Node v _ args) = (v, length args) : (concatMap goTerm args)

isVar :: Term a -> Bool
isVar (Var _) = True
isVar _       = False

isSubterm :: Eq a => Term a -> Term a -> Bool
isSubterm subterm = go
  where
    go t@(Var _)       = t == subterm
    go t@(Node _ _ ts) = (t == subterm) || (any go ts)

subterms :: Term a -> [Term a]
subterms = go
  where
    go t@(Var _)       = [t]
    go t@(Node _ _ ts) = t : (concatMap go ts)

definedSymbols :: Trs a -> [(Symbol,a)]
definedSymbols (Trs rules) = mapMaybe goRule rules
  where
    goRule (Rule lhs _) = goTerm lhs
    goTerm (Var _)      = Nothing
    goTerm (Node s l _) = Just (s,l)

dependecyPair :: Eq a => Trs a -> Trs (a, Bool)
dependecyPair (Trs rules) = Trs $ concatMap goRule rules
  where
    defined = definedSymbols $ Trs rules

    goRule (Rule     (Var _)          _  ) = []
    goRule (Rule lhs@(Node ls ll lts) rhs) = do
      (Node us ul uts) <- us
      return $ Rule (Node ls (ll,True) $ map (markSymbol False) lts)
                    (Node us (ul,True) $ map (markSymbol False) uts)
      where
        us = do s@(Node f l _) <- filter (not . isVar) $ subterms rhs
                guard $ (f,l) `elem` defined
                guard $ not $ isSubterm s lhs
                return s

    markSymbol _ (Var v)       = Var v
    markSymbol m (Node s l ts) = Node s (l,m) $ map (markSymbol m) ts
