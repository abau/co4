{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Thesis.LoopBenchmark where

import           System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tuple (swap)
import qualified TPDB.Input.File as TPDB
import qualified TPDB.Data as TPDB
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Thesis.LoopStandalone

symBits      = 2
varBits      = 1
maxTermChild = 3
maxTermDepth = 2
maxSteps     = 3

$( compileFile [Cache,ImportPrelude] "test/CO4/Thesis/LoopStandalone.hs" )

allocator = knownLoopingDerivation (uList maxSteps uStep) uPos uSubst
  where
    uList 0 _ = knownNill
    uList i a = union knownNill $ knownConss a $ uList (i - 1) a

    uUnary 0  = knownZ
    uUnary i  = union knownZ $ knownS $ uUnary $ i - 1

    uPos      = uList maxTermDepth $ uUnary $ maxTermChild - 1

    uVar      = uNat varBits
    uSym      = uNat symBits

    uTerm     = go maxTermDepth
      where
        go 0  = union (knownVar uVar) (knownNode uSym knownNill)
        go i  = union (knownVar uVar) (knownNode uSym $ uList maxTermChild $ go $ i - 1)

    uRule     = knownPair uTerm uTerm

    uSubst :: TAllocator (List (Pair Nat Term))
    uSubst    = foldr (\var -> knownConss (knownPair (fromKnown var) uTerm)) knownNill 
                      $ map nat [0 .. (2^varBits)-1]

    uStep     = knownStep uTerm
                          uRule
                          uPos
                          uSubst
                          uTerm

makeSigMap :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> M.Map TPDB.Identifier Nat
makeSigMap (TPDB.RS sig _ _) = M.fromList $ zip sig $ map nat [0..]

makeVarMap :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> M.Map TPDB.Identifier Nat
makeVarMap (TPDB.RS _ rules _) = M.fromList $ zip (S.toList vars) $ map nat [0..]
  where
    vars = S.unions $ map varsInRule rules

    varsInRule r = case TPDB.relation r of
      TPDB.Strict -> S.union (TPDB.vars $ TPDB.lhs r) (TPDB.vars $ TPDB.rhs r)
      _ -> error "supports only strict relations"

mapTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> List (Pair Term Term)
mapTrs trs = 
  if 2 ^ symBits > M.size sigMap
  then error "too many symbols"
  else if 2 ^ varBits > M.size varMap
       then error "too many variables"
       else foldr Conss Nill $ map rule $ TPDB.rules trs
  where
    sigMap = makeSigMap trs
    varMap = makeVarMap trs

    rule r       = Pair (term 0 $ TPDB.lhs r) (term 0 $ TPDB.rhs r)
    term depth t = 
      if depth > maxTermDepth
      then error "term too deep"
      else case t of
        TPDB.Var v     -> Var $ varMap M.! v
        TPDB.Node s ts -> if length ts > maxTermChild
                          then error "too many children"
                          else Node (sigMap M.! s) $ foldr Conss Nill $ map (term $ depth + 1) ts

pretty :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> LoopingDerivation -> String
pretty trs (LoopingDerivation steps pos subst) = unwords [ "LoopingDerivation"
                                                         , prettyList prettyStep steps
                                                         , prettyList prettyUnary pos
                                                         , prettyList prettySubst subst
                                                         ]
  where
    sigMap = M.fromList $ map swap $ M.toList $ makeSigMap trs
    varMap = M.fromList $ map swap $ M.toList $ makeVarMap trs

    prettyStep (Step t0 rule pos subst t1) =
      unwords [ "Step"
              , parens $ prettyTerm t0
              , parens $ prettyRule rule
              , prettyList prettyUnary pos
              , prettyList prettySubst subst
              , parens $ prettyTerm t1
              ]

    prettyUnary = show . go
      where go Z     = 0
            go (S u) = 1 + (go u)

    prettySubst (Pair var term) = concat ["(", prettyVar var, ", ", prettyTerm term, ")"]

    prettyRule (Pair l r) = unwords [ prettyTerm l, "->", prettyTerm r ]

    prettyVar v = TPDB.name $ varMap M.! v
    prettySym s = TPDB.name $ sigMap M.! s

    prettyTerm t = case t of Var v -> "Var " ++ (prettyVar v)
                             Node f ts -> unwords [ "Node"
                                                  , prettySym f
                                                  , prettyList prettyTerm ts
                                                  ]

    prettyList f xs = concat ["[", go (map' f xs), "]"]
      where
        go Nill = ""
        go (Conss y ys) = case ys of
          Nill -> y
          Conss {} -> concat [y, ", ", go ys]

    parens x = concat [ "(", x, ")" ]

mainFile :: FilePath -> IO ()
mainFile file = TPDB.get file >>= \case
  Right _ -> error "SRS not supported"
  Left trs -> co4Result >>= putStrLn . show . fmap (pretty trs)
    where
      co4Result = solveAndTestP (mapTrs trs) allocator encConstraint constraint

main :: IO ()
main = getArgs >>= \[file] -> mainFile file
