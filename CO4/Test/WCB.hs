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
                        --,Profile
                        -- ,DumpAll "/tmp/WCB"
                        , Cache
                        ] 
         $ compileFile "CO4/Test/WCB.standalone.hs" )


uBase = constructors [ Just [], Just [], Just [], Just []]

kList 0 a = known 0 2 []
kList i a = known 1 2 [ a , kList (i-1) a]

inforna cs = map ( \ c -> case c of
    '(' -> Open ; '.' -> Blank ; ')' -> Close ) cs

ex1 = inforna "(((((...(((((......))))).)))))"

ex0 = [Open,Open
     ,Blank,Close,Open
     ,Close ,Close,Blank 
    ]

-- allocator = kList size uBase

result_for sec = 
    solveAndTestBooleanP sec booleanCache (kList (length sec) uBase) encMain main

mainz = do
    hSetBuffering stdout LineBuffering
    [ arg1 ] <- getArgs
    result_for $ inforna arg1
