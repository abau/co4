{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.LPO where

import qualified Prelude 
import Prelude (($), (-), (*),(==))

import qualified Data.Maybe as M

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( compileFile [] "CO4/Test/LPO.standalone.hs" )

uSymbol = constructors [ M.Just [], M.Just [], M.Just [], M.Just [] ]

uList 0 _  = constructors [ M.Just [] , M.Nothing ]
uList i a  = constructors [ M.Just [] , M.Just [a, uList (i-1) a ] ]

result = solveAndTestP trs2 (uList 3 uSymbol) encMain main

trs1 = Cons (Pair (Term F (Cons (Var X) (Cons (Term E Nil) Nil)))
                  (Var X))
      (Cons (Pair (Term I (Cons (Term E Nil) Nil))
                  (Term E Nil))

      (Cons (Pair (Term I (Cons (Term F (Cons (Var X) (Cons (Var Y) Nil))) Nil))
                  (Term F (Cons (Term I (Cons (Var Y) Nil)) 
                          (Cons (Term I (Cons (Var X) Nil)) Nil))))

      (Cons (Pair (Term F (Cons (Term F (Cons (Var X) (Cons (Var Y) Nil)))
                          (Cons (Var Z) Nil)))
                  (Term F (Cons (Var X)
                          (Cons (Term F (Cons (Var Y) (Cons (Var Z) Nil))) Nil))))

      Nil)))

trs2 = Cons (Pair (Term E (Cons (Var X) Nil)) (Term F (Cons (Var X) Nil)))
      (Cons (Pair (Term F (Cons (Var X) Nil)) (Term I (Cons (Var X) Nil)))
      (Cons (Pair (Term I (Cons (Var X) Nil)) (Term J (Cons (Var X) Nil)))
      Nil))

trs3 = Cons (Pair (Term E (Cons (Var X) Nil)) (Term F (Cons (Var X) Nil)))
      (Cons (Pair (Term F (Cons (Var X) Nil)) (Term I (Cons (Var X) Nil)))
      (Cons (Pair (Term I (Cons (Var X) Nil)) (Term J (Cons (Var X) Nil)))
      (Cons (Pair (Term J (Cons (Var X) Nil)) (Term E (Cons (Var X) Nil)))
      Nil)))

