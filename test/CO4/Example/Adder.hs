{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.Adder
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d| halfAdder :: Bool -> Bool -> (Bool,Bool)
       halfAdder a b = (xor2 a b, a && b)

       fullAdder :: Bool -> Bool -> Bool -> (Bool,Bool)
       fullAdder a b c = case halfAdder a b of
         (s1,c1) -> case halfAdder s1 c of
           (s2,c2) -> (s2, c1 || c2)

       constraint :: Bool -> (Bool,Bool) -> Bool
       constraint c (a,b) = not $ snd $ fullAdder a b c

   |] >>= compile [ImportPrelude, Cache]
  )

result = solveAndTestP True complete encConstraint constraint
