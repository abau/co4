{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.Loop where

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
                        -- , DumpAll "/tmp/sl" 
                        ] 
         $ compileFile "CO4/Test/Loop.standalone.hs" )


kList 0 _  = known 0 2 []
kList i a  = known 1 2 [ a , kList (i-1) a ]

cSymbol xs = case xs of
    [] -> known 0 2 []
    x:xs' -> 
        known 1 2 [ known (fromEnum x) 2 []
                  , cSymbol xs' 
                  ]

-- should be kList !
uSymbol bits = uList bits uBool

-- this is indeed uList
uWord width bits = uList width (uSymbol bits)

uRule width bits = 
    known 0 1 [ uWord width bits, uWord width bits ]

uStep rwidth bits wwidth =
    known 0 1 [ uWord wwidth bits
              , uRule rwidth bits
              , uWord wwidth bits
              ]

uDeriv rwidth bits wwidth dlength =
    uList dlength ( uStep rwidth bits wwidth)

toBin :: Int -> [Bool]
toBin x = 
    if x == 0 then [] 
    else let (d,m) = divMod x 2 in odd m : toBin d

fromBin :: [Bool] -> Int
fromBin xs = foldr ( \ x y -> fromEnum x + 2*y ) 0 xs


solve filePath width height = do
  sys <- TPDB.get_srs filePath

  let sigma = nub $ concat 
            $ map (\ u  -> TPDB.lhs u ++ TPDB.rhs u) $ TPDB.rules sys
      m = M.fromList $ zip sigma $ map toBin [ 0 .. ]
      f xs = map (m M.!) xs
      srs = map 
        ( \ u -> ( f $ TPDB.lhs u, f $ TPDB.rhs u ) ) $ TPDB.rules sys

  let bits = length ( toBin (length sigma - 1))
      rwidth = maximum $ do
          (l,r) <- srs ; [ length l, length r ]
  solution <- solveAndTestBooleanP 
      srs (uDeriv rwidth bits width height)
      encMain main

  case solution of
    Nothing -> return ()
    Just s  -> print s


