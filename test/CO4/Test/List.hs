{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CO4.Test.List
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),(-))
import           Data.Maybe
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool   = False | True
       data List a = Nil   | Cons a (List a)

       fold cons nil xs = case xs of
         Nil       -> nil
         Cons y ys -> cons y (fold cons nil ys)

       and2 x y = case x of
         False -> False
         True  -> y
 
       and = fold and2 True

       main xs = and xs

   |] >>= compile []
  )

uBool      = constructors [ Just [] , Just [] ]

uList 0 _  = constructors [ Just [] , Nothing ]
uList i a  = constructors [ Just [] , Just [a, uList (i-1) a ] ]

kNil       = known 0 2 []
kCons x xs = known 1 2 [ x , xs ]
kList 0 _  = kNil
kList i a  = kCons a (kList (i-1) a)

allocator1 = uList 3 uBool
allocator2 = kList 3 uBool

result = solveAndTest allocator2 encMain main
