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
import           CO4 hiding (solve)
import           CO4.Prelude

import Data.List ( nub )
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import Control.Monad ( forM,void )
import System.Environment
import GHC.Word (Word8)

import qualified TPDB.Data as TPDB
import qualified TPDB.Input as TPDB
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB
import qualified TPDB.Plain.Read as TPDB

import qualified Text.PrettyPrint.Leijen as PP

import System.Console.GetOpt

$( runIO $ configurable [ Verbose
                        , ImportPrelude
                        -- , DumpAll "/tmp/sl" 
                        -- , Profile
                        , Cache
                        ] 
         $ compileFile "CO4/Test/SLPO.standalone.hs" )


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

uModel sym_bits model_bits = uTree sym_bits 
                           $ uTree model_bits 
                           $ kList model_bits uBool

uQuasiPrec bits_for_symbols = 
    known 0 1 [ uBool
              , uTree bits_for_symbols $ uBool
              , uTree bits_for_symbols $ uNat bits_for_symbols 
              ]

uLab conf srs =
    let sigma = nub $ do (l,r) <- srs ;  l ++ r
        width = maximum $ do (l,r) <- srs; map length [l,r]
        bits_for_symbols = maximum $ map length sigma 
    in  known 0 1 
           [ uModel bits_for_symbols (bits_for_model conf)
           , kList (number_of_interpretations conf)
                  $ uQuasiPrec (bits_for_symbols + bits_for_model conf)
           , kList (length srs) uBool
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


data Config =
     Config { bits_for_model :: Int
             , number_of_interpretations :: Int
             , dimension_for_matrices :: Int
             , bits_for_numbers :: Int
             }
    deriving Show

config0 = Config
         { bits_for_model = 1 
         , number_of_interpretations = 2
         , dimension_for_matrices = 1
         , bits_for_numbers = 8
         }

options = [ Option ['m'] ["model"]
             (ReqArg ( \ s conf -> conf { bits_for_model = read s } ) "Int")
             "bits for model"
          , Option ['i'] ["interpretations"]
             (ReqArg ( \ s conf -> conf { number_of_interpretations = read s } ) "Int")
             "number of interpretations"
          , Option ['d'] ["dimension"]
             (ReqArg ( \ s conf -> conf { dimension_for_matrices = read s } ) "Int")
             "(arctic) matrix dimension"
          ]


main = do
    argv <- getArgs
    case getOpt Permute options argv of
        (c,[f],[]) -> do
            let conf = foldl (flip id) config0 c
            solve conf f
        (_,_,errs) -> 
            ioError $ userError $ concat errs ++ usageInfo "SL" options

example = case TPDB.srs "(RULES a a -> a b a)" of 
    Right sys -> solveTPDB config0 sys

solve conf filePath = TPDB.get_srs filePath >>= solveTPDB conf

solveTPDB conf sys = do

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

      bdt2int' t = M.fromList $ do
                  (xs, w) <- M.toList $ bdt2map t
                  v <- maybeToList $ M.lookup xs m'
                  return ( v, w )

      bdt2mod t = M.fromList $ do
                  (k,v) <- M.toList $ bdt2map t
                  return ( fromBin k, fromBin v )

      bdt2labelled_int t = M.fromList $ do
          (xs, mat) <- M.toList $ bdt2map t
          let (pre,post) = splitAt bits_for_symbols xs
          v <- maybeToList $ M.lookup pre m'
          return ((v, fromBin post), mat)


  print $ TPDB.pretty sys
  print conf
  -- print srs
  -- print m

  let alloc = uLab conf srs
  solution <- solveAndTestP 
      srs 
      alloc encConstraint constraint

  case solution of
    Nothing -> return ()
    Just (Label mod ints remove) -> do
        void $ forM (M.toList $ bdt2int mod) (print . PP.pretty)
        void $ forM ints $ \ (QP dir del ord) -> 
            print $ PP.pretty (dir, bdt2labelled_int del
                                  , bdt2labelled_int ord )
        print $ TPDB.pretty ( zip (TPDB.rules sys) remove )
        
-- * pretty printers

instance PP.Pretty Nat where pretty = PP.text . show 

instance PP.Pretty Direction where pretty = PP.text . show

instance (PP.Pretty k, PP.Pretty v) => PP.Pretty (M.Map k v) where
    pretty m = PP.pretty $ M.toList m

instance PP.Pretty TPDB.Identifier where pretty = PP.text . show

