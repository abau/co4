{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.SL where

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4 hiding (solve)
import           CO4.Prelude

import Data.List ( nub )
import qualified Data.Map as M
import Control.Monad ( forM,void )

import qualified TPDB.Data as TPDB
import qualified TPDB.Input as TPDB
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB
import qualified TPDB.Plain.Read as TPDB

$( runIO $ configurable [ Verbose
                        , ImportPrelude
                        -- , DumpAll "/tmp/sl" 
                        , Profiling
                        ] 
         $ compileFile "CO4/Test/SL.standalone.hs" )


kList 0 _  = known 0 2 []
kList i a  = known 1 2 [ a , kList (i-1) a ]

uBDT bits leaf = 
    let t depth = if depth > 0
                 then known 1 2 [ t (depth-1), t (depth-1) ]
                 else known 0 2 [ leaf ]
    in  t bits

cSymbol xs = case xs of
    [] -> known 0 2 []
    x:xs' -> 
        known 1 2 [ known (fromEnum x) 2 []
                  , cSymbol xs' 
                  ]

uSymbol bits = uList bits uBool
uWord len bits = uList len (uSymbol bits)
uRule len bits = known 0 1 [ uWord len bits, uWord len bits ]
uSRS rules len bits = uList rules ( uRule len bits )


uModel syms bits = case syms of
    [] -> known 0 2 []
    s:ss -> known 1 2 [ known 0 1 [ cSymbol s
                                  , uBDT bits (kList bits uBool)
                                  ] 
                      , uModel ss bits ]


uNat bits = kList bits uBool

uArctic bits = 
    constructors [ Just [], Just [ uNat bits] ]

uMatrix dim bits = 
    kList dim $ kList dim $ uArctic bits

uInter bits_for_symbols dim bits_for_numbers = 
   uBDT bits_for_symbols ( uMatrix dim bits_for_numbers )

uStep srs dim bits_for_number = 
    let sigma = nub $ do (l,r) <- srs ;  l ++ r
        width = maximum $ do (l,r) <- srs; map length [l,r]
        bits_for_symbols = maximum $ map length sigma 
    in  known 0 1 [ uInter bits_for_symbols dim bits_for_number 
                  , uSRS (length srs) width bits_for_symbols
                  ]

toBin :: Int -> [Bool]
toBin x = 
    if x == 0 then [] else let (d,m) = divMod x 2 in odd m : toBin d

toBin' :: Int -> Int -> [Bool]
toBin' w x = 
    if w > 0 then let (d,m) = divMod x 2 in odd m : toBin' (w-1) d
    else if x == 0 then [] else error "toBin: not enough bits"

fromBin :: [Bool] -> Int
fromBin xs = foldr ( \ x y -> fromEnum x + 2*y ) 0 xs

alphabet :: TPDB.SRS TPDB.Identifier -> [ TPDB.Identifier ]
alphabet sys = nub $ concat 
            $ map (\ u  -> TPDB.lhs u ++ TPDB.rhs u) $ TPDB.rules sys 


example = case TPDB.srs "(RULES a -> b)" of Right sys -> solveTPDB sys

solve filePath = TPDB.get_srs filePath >>= solveTPDB

solveTPDB sys = do

  let sigma = alphabet sys
      bits_for_symbols = length $ toBin $ length sigma - 1
      m = M.fromList $ zip sigma $ map (toBin' bits_for_symbols) [ 0 .. ]
      m' = M.fromList $ zip (map toBin [0..]) sigma
      f xs = map (m M.!) xs ; f' xs = map (m' M.!) xs
      srs = map 
        ( \ u -> ( f $ TPDB.lhs u, f $ TPDB.rhs u ) ) $ TPDB.rules sys

  print $ TPDB.pretty sys
  print srs
  print m

  let dim = 1 ; bits = 5
  solution <- solveAndTestBooleanP 
      srs (uStep srs dim bits)
      encMain main
{-
  let syms = nub $ do (l,r) <- srs ; l ++ r
  solution <- solveAndTestBooleanP srs (uModel syms 2) encMain main
-}

  case solution of
    Nothing -> return ()
    Just s  -> print s
        


        

