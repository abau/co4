{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Example.LoopSrs
where

import           System.Environment
import           Control.Monad ( void, forM )
import           Data.List (nub)
import qualified Data.Map as M
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4 hiding (solve)
import           CO4.Prelude
import qualified TPDB.Data as TPDB
import qualified TPDB.Input as TPDB
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB
import           CO4.Example.LoopSrs.Standalone

$( compileFile [ ImportPrelude, Cache ] "test/CO4/Example/LoopSrs/Standalone.hs" )

cSymbol xs = case xs of
    [] -> known 0 2 []
    x:xs' -> 
        known 1 2 [ known (fromEnum x) 2 []
                  , cSymbol xs' 
                  ]

-- should be kList !
uSymbol bits = uList bits complete

-- this is indeed uList
uWord width bits = uList width (uSymbol bits)

uRule width bits = 
    knownTuple2 (uWord width bits) (uWord width bits)

uStep rwidth bits wwidth =
    knownStep (uWord wwidth bits)
              (uRule rwidth bits)
              (uWord wwidth bits)

uDeriv rwidth bits wwidth dlength =
    uList dlength ( uStep rwidth bits wwidth)

uLoopDeriv rwidth bits wwidth dlength =
    knownLooping_Derivation (uWord wwidth bits)
                            (uDeriv rwidth bits wwidth dlength)
                            (uWord wwidth bits)

toBin :: Int -> [Bool]
toBin x = 
    if x == 0 then [] 
    else let (d,m) = divMod x 2 in odd m : toBin d

fromBin :: [Bool] -> Int
fromBin xs = foldr ( \ x y -> fromEnum x + 2*y ) 0 xs

main = do
    [ w, h, fp ] <- getArgs
    solve (read w) (read h) fp

solve  width height filePath = do
  sys <- TPDB.get_srs filePath

  let sigma = nub $ concat 
            $ map (\ u  -> TPDB.lhs u ++ TPDB.rhs u) $ TPDB.rules sys
      m     = M.fromList $ zip sigma $ map toBin [ 0 .. ]
      m'    = M.fromList $ zip (map toBin [ 0 .. ]) sigma
      f xs  = map (m M.!) xs
      def   = head sigma 
      f' xs = map (\ x -> M.findWithDefault def x m') xs

      back (Step pre (l,r) post) = ( f' pre, (f' l, f' r), f' post )

      srs = map 
        ( \ u -> ( f $ TPDB.lhs u, f $ TPDB.rhs u ) ) $ TPDB.rules sys

  let bits = length ( toBin (length sigma - 1))
      rwidth = maximum $ do
          (l,r) <- srs ; [ length l, length r ]

  solution <- solveAndTestP 
      srs 
      (uLoopDeriv rwidth bits width height)
      encConstraint constraint

  case solution of
    Nothing -> return ()
    Just (Looping_Derivation pre steps suf)  -> 
        void $ forM steps (print . back)

  return solution
