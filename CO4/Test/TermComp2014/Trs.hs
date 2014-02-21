{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Trs
  (parseTrs, assignments, modelAllocator)
where

import           Control.Exception (assert)
import           Data.Char (ord)
import           Data.List (nub)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified TPDB.Data as TPDB
import qualified TPDB.Plain.Read as Read
import           CO4.AllocatorData (Allocator,known)
import           CO4.Prelude (kNil,kList',uBool,kBool,uTuple2)
import           CO4.Util (toBinary)
import           CO4.Test.TermComp2014.Standalone

{-
import TPDB.Plain.Write ()
import TPDB.Pretty (pretty)
import Debug.Trace
-}

type TPDBTerm = TPDB.Term Char Char
type TPDBTrs  = TPDB.TRS  Char Char

parseTrs :: FilePath -> IO Trs
parseTrs path = readFile path >>= return . Read.trs >>= \case
  Left msg  ->    error msg
  Right trs -> do {-putStrLn (show $ pretty trs)-}
                  return $ goTrs trs
  where
    goTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> Trs
    goTrs                     = Trs . map goRule . TPDB.rules
    goRule rule               = Rule (goTerm $ TPDB.lhs rule) (goTerm $ TPDB.rhs rule)
    goTerm (TPDB.Var v)       = Var $ goIdentifier v
    goTerm (TPDB.Node v args) = Node (goIdentifier v) $ map goTerm args

    goIdentifier i | length (TPDB.name i) == 1 = charToSymbol $ head $ TPDB.name i

assignments :: Int -> Trs -> Assignments
assignments n trs = do 
  values <- sequence $ replicate (length vars) [0..(2^n)-1]
  return $ zipWith goMapping vars values
  where
    vars                 = goTrs trs
    goTrs (Trs rules)    = nub $ concatMap goRule rules
    goRule (Rule l r)    = goTerm l ++ (goTerm r)
    goTerm (Var v)       = [v]
    goTerm (Node _ args) = concatMap goTerm args

    goMapping v value = (v, toBinary (Just n) value)

modelAllocator :: Int -> Trs -> Allocator
modelAllocator n (Trs rules) = kList' $ map goArity $ M.toList arities
  where
    arities = M.fromListWith (\a b -> assert (a == b) a) $ concatMap goRule rules
      where goRule (Rule l r)    = (goTerm l) ++ (goTerm r)
            goTerm (Var {})      = []
            goTerm (Node v args) = (v, length args) : (concatMap goTerm args)

    goArity (v,arity)      = uTuple2 (symbolAllocator v) (goInterpretation arity)
    goInterpretation arity = kList' $ do args <- sequence $ replicate arity [0..numStates - 1]
                                         return $ goMapping args
      where
        numStates = 2^n
    
    goMapping args = uTuple2 (kList' $ map (knownValueAllocator n) args)
                             (unknownValueAllocator n)

charToSymbol :: Char -> Symbol
charToSymbol = toBinary Nothing . ord

knownValueAllocator :: Int -> Int -> Allocator
knownValueAllocator n l = kList' $ map kBool $ toBinary (Just n) l
      
unknownValueAllocator :: Int -> Allocator
unknownValueAllocator n = kList' $ replicate n uBool

symbolAllocator :: Symbol -> Allocator
symbolAllocator = kList' . map kBool
