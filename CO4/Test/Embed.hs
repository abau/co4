{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CO4.Test.Embed
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),(-))
import           Data.Maybe
import qualified GHC.Types
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool = False | True
       data List a = Nil | Cons a (List a)

       eqBool x y = case x of
        False -> case y of False -> True
                           True -> False
        True  -> case y of False -> False
                           True -> True

       and x y = case x of
        False -> False
        True  -> y

       f xs ys = case xs of
        Nil -> True
        Cons x xs' -> case ys of 
          Nil -> False
          Cons y ys' -> case eqBool x y of
                          False -> f xs ys'
                          True  -> f xs' ys'

       main x = and (f (Cons False (Cons False (Cons False Nil))) x)
                    (f (Cons True  (Cons False (Cons True  Nil))) x)

   |] >>= runIO . configurable [Verbose, DumpAfter "satchmoUnqualified" ""] . compile 
  )


uBool      = constructors [ Just [] , Just [] ]

uList 0 _  = constructors [ Just [] , Nothing ]
uList i a  = constructors [ Just [] , Just [a, uList (i-1) a ] ]

kNil       = known 0 2 []
kCons x xs = known 1 2 [ x , xs ]
kList 0 _  = kNil
kList i a  = kCons a (kList (i-1) a)

allocator = kList 8 uBool

result = solveAndTest allocator encMain main
