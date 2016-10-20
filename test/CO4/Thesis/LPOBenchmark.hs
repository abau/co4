{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
import           System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified TPDB.Input.File as TPDB
import qualified TPDB.Data as TPDB
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Thesis.LPOStandalone
import           CO4.Util (bitWidth)

$( compileFile [Cache, ImportPrelude] "test/CO4/Thesis/LPOStandalone.hs" )

allocator :: Int -> TAllocator (List Nat)
allocator numSym = kList numSym $ uNat $ bitWidth numSym
  where
    kList 0 _ = knownNill
    kList i a = knownConss a $ kList (i - 1) a

type IdentifierMap = M.Map TPDB.Identifier Nat

makeSymMap :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> IdentifierMap
makeSymMap (TPDB.RS sig _ _) = M.fromList $ zip sig $ map nat [0..]

makeVarMap :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> IdentifierMap
makeVarMap (TPDB.RS _ rules _) = M.fromList $ zip vars $ map nat [0..]
  where
    vars = S.toList $ S.unions $ map go rules
      where
        go r = case TPDB.relation r of
          TPDB.Strict -> S.union (TPDB.vars $ TPDB.lhs r) (TPDB.vars $ TPDB.rhs r)
          _ -> error "supports only strict relations"

mapTrs :: IdentifierMap -> IdentifierMap -> TPDB.TRS TPDB.Identifier TPDB.Identifier -> TRS
mapTrs symMap varMap trs = TRS (foldr Conss Nill $ map snd $ M.toList symMap)
                               (foldr Conss Nill $ map rule $ TPDB.rules trs)
  where
    rule r                = Pair (term $ TPDB.lhs r) (term $ TPDB.rhs r)
    term (TPDB.Var v)     = Var $ varMap M.! v
    term (TPDB.Node s ts) = Node (symMap M.! s) $ foldr Conss Nill $ map term ts

mainFile :: FilePath -> IO ()
mainFile file = TPDB.get file >>= \case
  Right _ -> error "SRS not supported"
  Left trs -> solveAndTestP trs' alloc encConstraint constraint >>= \case
                 Nothing -> putStrLn "Nothing"
                 Just ld -> putStrLn "YES"
    where
      symMap    = makeSymMap trs
      varMap    = makeVarMap trs
      trs'      = mapTrs symMap varMap trs
      alloc     = allocator $ length $ TPDB.signature trs

main :: IO ()
main = getArgs >>= \[file] -> mainFile file
