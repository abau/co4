{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Util
where

import           Control.Exception (assert)
import           Data.Char (ord,isAsciiLower)
import           Data.List (nub)
import qualified Data.Map as M
import           Unsafe.Coerce
import qualified TPDB.Data as TPDB
import qualified TPDB.Plain.Read as Read
import           CO4.Util (toBinary, binaries)
import           CO4.Test.TermComp2014.Standalone 
  (Symbol,Assignments,Trs(..),Rule(..),Term(..),Nat (..))

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

fromNat :: Nat -> Int
fromNat Zero     = 0
fromNat (Succ x) = 1 + (fromNat x)
