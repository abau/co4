{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Example.WCB_Matrix
  (result,ex0)
where

import Prelude hiding (sequence)

import           Control.Monad (void,forM)
import           Language.Haskell.TH (runIO)
import qualified Data.Map as M
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude hiding (kList)
import           CO4.Util (toBinary,fromBinary)
import           System.Environment (getArgs)
import           System.IO
import           CO4.Example.WCB_MatrixStandalone

$( compileFile [ ImportPrelude
               -- , DumpAll "/tmp/WCB_Matrix"
               , InstantiationDepth 20
               ] 
  "test/CO4/Example/WCB_MatrixStandalone.hs" )

-- data Base = Base [Bool]
uBool = constructors [ Just [], Just [] ]
kList 0 _ = known 0 2 []
kList i a = known 1 2 [ a, kList (i-1) a ]

uBase = known 0 1 [ kList 2 uBool ]

-- balanced binary encoding (create prefix code)
-- balanced :: [a] -> Tree a
balanced xs f = case xs of
    [x] -> known 0 2 [ f x ]
    _ -> let (pre, post) = splitAt (div (length xs) 2)
                           xs
         in  known 1 2 [ balanced pre f 
                       , balanced post f
                       ]

uEnergy = constructors [ Just [] , Just [ toAllocator $ uNat 8 ]]
uEnergy2 = known 0 1 [ uEnergy, uEnergy ]

uMatrix xs ys f =
    let row xs g = case xs of
            []    -> known 0 2 [] 
            x:xs' -> known 1 2 [ g x, row xs' g ]
    in  row xs $ \ x -> row ys $ \ y -> f x y

-- upper triag finite energy
uTriagGap delta n = uMatrix [1 .. n] [1 .. n] $ \ i j ->
     if i + delta <= j 
     then known 1 2 [ toAllocator $ uNat 8 ]
     else known 0 2 []


uTriag2Gap delta n = uMatrix [1 .. n] [1 .. n] $ \ i j ->
     if i + delta <= j 
     then known 0 1 [ known 1 2 [ toAllocator $ uNat 8 ], uEnergy ]
     else known 0 1 [ known 0 2 [], known 0 2 [] ]

inforna cs = map ( \ c -> case c of
    '(' -> Open ; '.' -> Blank ; ')' -> Close ) cs

ex1 = inforna "(((((...(((((......))))).)))))"

ex0 = [ Open, Open
      , Blank, Close, Open
      , Close , Close, Blank 
      ]

result :: [Paren] -> IO (Maybe [Base])
result sec = do
    let n = length sec
    out <- solveAndTestP 
       sec 
       ( unsafeTAllocator $ known 0 1 [ kList n uBase
                                      --, uTriag2Gap 1 (n+1) 
                                        , uTriagGap 1 (n+1)
                                      ] )
       encConstraint
       constraint
    return $ fmap fst out
