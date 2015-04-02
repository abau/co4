{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.Thesis.Loop
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Test.Thesis.LoopStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/Thesis/LoopStandalone.hs" )

symBits      = 2
varBits      = 1
maxTermChild = 3
maxTermDepth = 2
maxSteps     = 3

vars    = [ nat varBits 0, nat varBits 1 ]
fSym    = nat symBits 0
gSym    = nat symBits 1
nullSym = nat symBits 2
oneSym  = nat symBits 3
toyama  = 
  let f a1 a2 a3 = Node fSym (Conss a1 (Conss a2 (Conss a3 Nill)))
      g a1 a2    = Node gSym (Conss a1 (Conss a2 Nill))
      null       = Node nullSym Nill
      one        = Node oneSym Nill
      x          = Var $ vars !! 0
      y          = Var $ vars !! 1
  in
      Conss (Pair (f null one x) (f x x x))
    ( Conss (Pair (g x y)        x)
    ( Conss (Pair (g x y)        y) 
      Nill ))

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
uSubst    = foldr (\var -> knownConss (knownPair (fromKnown var) uTerm)) knownNill vars

uStep     = knownStep uTerm
                      uRule
                      uPos
                      uSubst
                      uTerm

allocator = knownLoopingDerivation (uList maxSteps uStep)
                                   uPos
                                   uSubst

result = co4Result >>= return . fmap pretty
  where 
    co4Result = solveAndTestP toyama allocator encConstraint constraint

    pretty (LoopingDerivation steps pos subst) =
      unwords [ "LoopingDerivation"
              , prettyList prettyStep steps
              , prettyList prettyUnary pos
              , prettyList prettySubst subst
              ]
    
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

    prettyVar v | value v == 0 = "x"
    prettyVar v | value v == 1 = "y"

    prettySym s | value s == 0 = "f"
    prettySym s | value s == 1 = "g"
    prettySym s | value s == 2 = "0"
    prettySym s | value s == 3 = "1"

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
