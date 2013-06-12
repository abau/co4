{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.TermComp where

import           Data.List (nub)
import qualified Data.Map as M


import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4 hiding (solve)
import           CO4.Prelude

import qualified TPDB.Data as TPDB
import qualified TPDB.Input as TPDB
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB

$( runIO $ configurable [ Verbose
                        , ImportPrelude
                        , DumpAll "/tmp/sl"                          
                        ] 
         $ compileFile "CO4/Test/SL.standalone.hs" )


kList 0 _  = known 0 2 []
kList i a  = known 1 2 [ a , kList (i-1) a ]

cSymbol xs = case xs of
    [] -> known 0 2 []
    x:xs' -> 
        known 1 2 [ known (fromEnum x) 2 []
                  , cSymbol xs' 
                  ]

uNat bits = kList bits uBool

uArctic bits = 
    constructors [ Just [], Just [ uNat bits] ]

uMatrix dim bits = 
    kList dim $ kList dim $ uArctic bits

uInter syms dim bits = case syms of
    [] -> known 0 2 []
    s:ss -> 
        known 1 2 [ known 0 1 [ cSymbol s
                              , uMatrix dim bits 
                              ]
                  , uInter ss dim bits
                  ]

toBin :: Int -> [Bool]
toBin x = 
    if x == 0 then [] 
    else let (d,m) = divMod x 2 in odd m : toBin d

fromBin :: [Bool] -> Int
fromBin xs = foldr ( \ x y -> fromEnum x + 2*y ) 0 xs

solve filePath = do
  sys <- TPDB.get_srs filePath

  let sigma = nub $ concat 
            $ map (\ u  -> TPDB.lhs u ++ TPDB.rhs u) $ TPDB.rules sys
      m = M.fromList $ zip sigma $ map toBin [ 0 .. ]
      f xs = map (m M.!) xs
      srs = map 
        ( \ u -> ( f $ TPDB.lhs u, f $ TPDB.rhs u ) ) $ TPDB.rules sys

  let dim = 3 ; bits = 3
  solution <- solveAndTestBooleanP 
      srs (uInter (M.elems m) dim bits)
      encMain main

  case solution of
    Nothing -> return ()
    Just s  -> print s

