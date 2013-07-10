{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module CO4.Test.SL where
module Main where

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4 hiding (solve)
import           CO4.Prelude

import Data.List ( nub )
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import Control.Monad ( forM,void )
import System.Environment

import qualified TPDB.Data as TPDB
import qualified TPDB.Input as TPDB
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB
import qualified TPDB.Plain.Read as TPDB

$( runIO $ configurable [ Verbose
                        , ImportPrelude
                        -- , DumpAll "/tmp/sl" 
                        , Profile
                        , Cache
                        ] 
         $ compileFile "CO4/Test/SL.standalone.hs" )


uTree bits leaf = 
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

{-
uSymbol bits = uList bits uBool
uWord len bits = uList len (uSymbol bits)
uRule len bits = known 0 1 [ uWord len bits, uWord len bits ]
uSRS rules len bits = uList rules ( uRule len bits )
-}


uModel sym_bits model_bits = uTree sym_bits 
                           $ uTree model_bits 
                           $ kList model_bits uBool


uLab srs bits_for_model num_ints dim bits_for_numbers = 
    let sigma = nub $ do (l,r) <- srs ;  l ++ r
        width = maximum $ do (l,r) <- srs; map length [l,r]
        bits_for_symbols = maximum $ map length sigma 
    in  known 0 1 
           [ uModel bits_for_symbols bits_for_model
           , kList num_ints $ uInter (bits_for_symbols + bits_for_model) 
                                     dim bits_for_numbers
           , kList (length srs) uBool
           ]


uArctic bits = 
    constructors [ Just [], Just [ uNat bits] ]

uMatrix dim bits = 
    kList dim $ kList dim $ uArctic bits

uInter bits_for_symbols dim bits_for_numbers = 
   uTree bits_for_symbols ( uMatrix dim bits_for_numbers )


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


main = do
    [ f ] <- getArgs
    solve f

example = case TPDB.srs "(RULES a a -> a b a)" of Right sys -> solveTPDB sys

solve filePath = TPDB.get_srs filePath >>= solveTPDB

solveTPDB sys = do

  let sigma = alphabet sys
      bits_for_symbols = length $ toBin $ length sigma - 1
      m = M.fromList $ zip sigma $ map (toBin' bits_for_symbols) [ 0 .. ]
      m' = M.fromList $ zip (map (toBin' bits_for_symbols) [0..]) sigma
      f xs = map (m M.!) xs ; f' xs = map (m' M.!) xs
      srs = map 
        ( \ u -> ( f $ TPDB.lhs u, f $ TPDB.rhs u ) ) $ TPDB.rules sys

      bdt2map t = let h t xs = case t of
                          Leaf y -> [(xs, y)]
                          Branch l r -> h l (xs ++ [False]) ++ h r (xs ++ [True])
                  in  M.fromList $ h t []

      bdt2int t = M.fromList $ do
                  (xs, w) <- M.toList $ bdt2map t
                  v <- maybeToList $ M.lookup xs m'
                  return ( v, bdt2mod w )

      bdt2mod t = M.fromList $ do
                  (k,v) <- M.toList $ bdt2map t
                  return ( fromBin k, fromBin v )

      

          
  print $ TPDB.pretty sys
  print srs
  print m

  let alloc = uLab srs 2 -- bits_for_model
                       4 -- num_interpretations
                       1 -- dim for matrices
                       8 -- bits_for_numbers (in matrices)
  solution <- solveAndTestP 
      srs 
      alloc encConstraint constraint

  case solution of
    Nothing -> return ()
    Just (Label mod ints remove) -> do
        print $ bdt2int mod
        void $ forM ints $ \ int -> print $ bdt2map  int
        print $ TPDB.pretty ( zip (TPDB.rules sys) remove )
        



