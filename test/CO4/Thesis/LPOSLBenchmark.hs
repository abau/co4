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
import qualified TPDB.Pretty as TPDB
import qualified TPDB.Plain.Write as TPDB
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Thesis.LPOSLStandalone
import           CO4.Util (bitWidth)

$( compileFile [Cache, ImportPrelude] "test/CO4/Thesis/LPOSLStandalone.hs" )

carrierSize  = 1
carrier      = map nat [0 .. (carrierSize - 1)]

type IdentifierMap = M.Map TPDB.Identifier Nat

labels :: Int -> [[Nat]]
labels arity = sequence $ replicate arity carrier

allocator :: IdentifierMap -> TAllocator (Pair (Precedence (Labelled Symbol))
                                               (Interpretation Symbol))
allocator symMap = knownPair precAlloc interAlloc
  where
    precWidth = bitWidth $ M.size symMap
    maxArity  = maximum $ map TPDB.arity $ M.keys symMap

    kList :: [TAllocator a] -> TAllocator (List a)
    kList = unsafeTAllocator
              . foldr (\x xs -> known 1 2 [x, xs]) (known 0 2 [])
              . map toAllocator

    uList 0 _ = knownNill
    uList i a = union knownNill $ knownConss a $ uList (i - 1) a

    uLSymbol  = knownPair (uNat precWidth) (uList maxArity $ uNat $ fromInteger carrierSize)

    precAlloc  = kList
               $ concatMap (\a -> replicate (length $ labels a) uLSymbol)
               $ map TPDB.arity
               $ M.keys symMap

    interAlloc = kList
               $ map (\(identifier,n) -> 
                     knownPair (fromKnown n)
                   $ kList
                   $ map (\l -> knownPair (kList $ map fromKnown l) (uNat $ fromInteger carrierSize)) 
                   $ labels 
                   $ TPDB.arity identifier
                 )
               $ M.toList symMap

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

parameter :: IdentifierMap -> IdentifierMap -> TPDB.TRS TPDB.Identifier TPDB.Identifier
          -> Triple (TRS Symbol) (List (Labelled Symbol)) (List Sigma)
parameter symMap varMap trs = Triple trs' lsymbols assignments
  where
    trs'                  = Pair (list $ map snd $ M.toList symMap)
                                 (list $ map rule $ TPDB.rules trs)

    rule r                = Pair (term $ TPDB.lhs r) (term $ TPDB.rhs r)
    term (TPDB.Var v)     = Var $ varMap M.! v
    term (TPDB.Node s ts) = Node (symMap M.! s) $ list $ map term ts

    list = foldr Conss Nill

    lsymbols = list 
             $ concatMap (\(s,n) -> map (Pair n) $ map list $ labels $ TPDB.arity s)
             $ M.toList symMap

    assignments = list
                $ map (list . map (\(v,s) -> Pair v s))
                $ map (zip $ M.elems varMap)
                $ sequence 
                $ replicate (M.size varMap) carrier

mainFile :: FilePath -> IO ()
mainFile file = TPDB.get file >>= \case
  Right _ -> error "SRS not supported"
  Left trs -> solveAndTestP param alloc encConstraint constraint >>= \case
                 Nothing -> putStrLn "Nothing"
                 Just ld -> putStrLn "YES"
    where
      symMap    = makeSymMap trs
      varMap    = makeVarMap trs
      param     = parameter symMap varMap trs
      alloc     = allocator symMap

main :: IO ()
main = getArgs >>= \[file] -> mainFile file
