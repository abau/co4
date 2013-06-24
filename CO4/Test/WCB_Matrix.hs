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

import qualified Data.Map as M
import Control.Monad ( void, forM )

import System.Environment (getArgs)
import System.IO

$( runIO $ configurable [ ImportPrelude
                        , DumpAll "/tmp/WCB"
                        , Cache
                        , Profile
                        ] 
  $ compileFile "CO4/Test/WCB_Matrix.standalone.hs" )


kList 0 a = known 0 2 []
kList i a = known 1 2 [ a , kList (i-1) a]

-- data Base = Base [Bool]
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

uEnergy = constructors [ Just [] , Just [ uNat8 ]]

uTriag xs e = balanced xs $ \ x ->
              balanced xs $ \ y -> e

inforna cs = map ( \ c -> case c of
    '(' -> Open ; '.' -> Blank ; ')' -> Close ) cs

ex1 = inforna "(((((...(((((......))))).)))))"

ex0 = [ Open, Open
      , Blank, Close, Open
      , Close , Close, Blank 
      ]

result_for :: [Paren] -> IO ()
result_for sec = do
    out <- solveAndTestBooleanP 
       sec 
       ( booleanCache  .  profile )
       ( known 0 1 [ balanced sec ( const uBase )
                   , uTriag [1..length sec] uEnergy
                   ] )
       encConstraint
       constraint
    case out of
       Nothing -> print "Nothing"
       Just (p, m) -> do
           print $ elems p
           void $ forM ( elems m ) $ (print . elems)

main = do
    hSetBuffering stdout LineBuffering
    [ arg1 ] <- getArgs
    result_for $ inforna arg1
