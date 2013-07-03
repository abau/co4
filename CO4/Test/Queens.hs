{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)

import System.Environment (getArgs)
import System.IO

$( runIO $ configurable [ ImportPrelude
                        , Profile
                        -- ,DumpAll "/tmp/WCB"
                        ] 
         $ compileFile "CO4/Test/Queens.standalone.hs")

kList 0 a = known 0 2 []
kList i a = known 1 2 [ a , kList (i-1) a]

uNat w = kList w uBool

toBin :: Int -> [Bool]
toBin x = 
    if x == 0 then [] 
    else let (d,m) = divMod x 2 in odd m : toBin d

fromBin :: [Bool] -> Int
fromBin xs = foldr ( \ x y -> fromEnum x + 2*y ) 0 xs

result n = do
    hSetBuffering stdout LineBuffering
    let xs = toBin n ; w = length xs
    solution <- solveAndTestP 
       xs
       (kList n (uNat w))
       encMain main
    case solution of
        Nothing -> putStrLn "no placement"
        Just xs -> print (map fromBin xs)

mainz = do
    [s] <- getArgs
    result $ read s

