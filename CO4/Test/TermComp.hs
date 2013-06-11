{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.TermComp where

import qualified Prelude as P
import           Prelude (($), (-), (*), (.), (!!))

import qualified Data.Maybe as M
import qualified Data.List as L

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4 hiding (solve)
import           CO4.Util (bitWidth,binaries,fromBinary)
import qualified TPDB.Data as TPDB
import qualified TPDB.Input as TPDB
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB

$( runIO $ configurable [Verbose] $ compileFile "CO4/Test/TermComp.standalone.hs" )

uBool      = constructors [ M.Just [], M.Just [] ]

uList 0 _  = constructors [ M.Just [] , M.Nothing ]
uList i a  = constructors [ M.Just [] , M.Just [a, uList (i-1) a ] ]

kList 0 _  = known 0 2 []
kList i a  = known 1 2 [ a , kList (i-1) a ]

uSymbol i = kList i uBool

solve filePath = do
  trs <- TPDB.get_trs filePath

  let l =            P.length $ symsOfTrs trs
      w = bitWidth $ P.length $ symsOfTrs trs


  P.putStrLn $ P.show $ TPDB.pretty trs
  P.putStrLn $ P.show $ varsOfTrs trs
  P.putStrLn $ P.show $ symsOfTrs trs

  solution <- solveAndTestBooleanP (mapTrs trs) (uList l (uSymbol w)) encMain main

  case solution of
    M.Nothing -> P.return ()
    M.Just s  -> P.putStrLn $ P.concat [ "Mapped solution: "
                                       , P.show $ map (mapSymbol' trs) s
                                       ]

--mapTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> C.TRS
mapTrs trs = mapList $ P.map mapRule $ TPDB.rules trs
  where
    mapRule (TPDB.Rule lhs rhs _ _) = Pair (mapTerm lhs) (mapTerm rhs)

    mapTerm term = case term of
      TPDB.Var  v       -> Var $ lookup v vMapping
      TPDB.Node s terms -> Term (lookup s sMapping) $ mapList $ P.map mapTerm terms

    vMapping    = P.zip vars (binaries' $ bitWidth $ P.length vars) 
    sMapping    = P.zip syms (binaries' $ bitWidth $ P.length syms)
    vars        = varsOfTrs trs
    syms        = symsOfTrs trs
    lookup x [] = Nil
    lookup x xs = mapList $ M.fromJust $ L.lookup x xs

    binaries' = P.map (P.map mapBool) . binaries

varsOfTrs = P.foldl1 L.union
          . P.map (\rule -> L.union (TPDB.lvars $ TPDB.lhs rule)
                                    (TPDB.lvars $ TPDB.rhs rule)) 
          . TPDB.rules
  
symsOfTrs = P.foldl1 L.union
          . P.map (\rule -> L.union (TPDB.lsyms $ TPDB.lhs rule)
                                    (TPDB.lsyms $ TPDB.rhs rule)) 
          . TPDB.rules 

mapSymbol' trs symbol = symsOfTrs trs !! (fromBinary $ P.map mapBool' $ mapList' symbol)

mapList :: [a] -> List a
mapList []     = Nil
mapList (x:xs) = Cons x $ mapList xs

mapList' :: List a -> [a]
mapList' Nil         = [] 
mapList' (Cons x xs) = x : (mapList' xs)

mapBool :: P.Bool -> Bool
mapBool P.False = False
mapBool P.True  = True

mapBool' :: Bool -> P.Bool
mapBool' False = P.False
mapBool' True  = P.True
