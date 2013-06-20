{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module CO4.Test.WCB where
module Main where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)

import System.Environment (getArgs)
import System.IO

$( runIO $ configurable [ImportPrelude
                        -- ,DumpAll "/tmp/WCB"
                        , Cache, Profile
                        ] 
  $ compileFile "CO4/Test/WCB_Matrix.standalone.hs" )


kList 0 a = known 0 2 []
kList i a = known 1 2 [ a , kList (i-1) a]

uBase = known 0 1 [ kList 2 uBool ]


-- balanced binary encoding (create prefix code)
-- balanced :: [a] -> Tree a
balanced xs f = case xs of
    [x] -> known 0 2 [ leaf x ]
    _ -> let (pre, post) = splitAt (div (length xs) 2)
                           xs
         in  known 1 2 [ balanced pre f 
                       , balanced post f
                       ]

uTriag xs e = balanced xs $ \ x ->
              balanced xs $ \ y -> 
              if x < y then e 
              else known 0 1 [] -- never accessed

inforna cs = map ( \ c -> case c of
    '(' -> Open ; '.' -> Blank ; ')' -> Close ) cs

ex1 = inforna "(((((...(((((......))))).)))))"

ex0 = [ Open, Open
      , Blank, Close, Open
      , Close , Close, Blank 
      ]

result_for sec = 
    solveAndTestBooleanP 
       sec 
       ( booleanCache . profile ) 
       ( known 0 1 [ kList (length sec) uBase
                   , uTriag [1..length sec] uEnergy
                   ] )
       encMain main

mainz = do
    hSetBuffering stdout LineBuffering
    [ arg1 ] <- getArgs
    result_for $ inforna arg1
