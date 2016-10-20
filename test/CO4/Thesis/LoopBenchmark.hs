{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
import           System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tuple (swap)
import qualified TPDB.Input.File as TPDB
import qualified TPDB.Data as TPDB
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude.Nat
import           CO4.Thesis.LoopStandalone
import           CO4.Util (bitWidth)

maxTermChild   = 3
maxTermDepth   = 3
maxSteps       = 3

$( compileFile [Cache,ImportPrelude] "test/CO4/Thesis/LoopStandalone.hs" )

data Properties = Properties { syms         :: [TPDB.Identifier]
                             , vars         :: [TPDB.Identifier]
                             , numTermChild :: Int
                             , numTermDepth :: Int
                             }

properties :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> Properties
properties (TPDB.RS sig rules _) = Properties sig vars numTermChild numTermDepth
  where
    vars = S.toList $ S.unions $ map go rules
      where
        go r = case TPDB.relation r of
          TPDB.Strict -> S.union (TPDB.vars $ TPDB.lhs r) (TPDB.vars $ TPDB.rhs r)
          _ -> error "supports only strict relations"

    numTermChild = maximum $ map (\r -> max (go $ TPDB.lhs r) (go $ TPDB.rhs r)) rules
      where
        go (TPDB.Var _)     = 0
        go (TPDB.Node _ ts) = maximum $ (length ts) : (map go ts)

    numTermDepth = maximum $ map (\r -> max (go $ TPDB.lhs r) (go $ TPDB.rhs r)) rules
      where
        go (TPDB.Var _)     = 0
        go (TPDB.Node _ []) = 0
        go (TPDB.Node _ ts) = 1 + (maximum $ map go ts)

checkValidity :: Properties -> IO ()
checkValidity p = case ( maxTermChild >= numTermChild p
                       , maxTermDepth >= numTermDepth p
                       ) of
  (False, _) -> error $ "too many children " ++ (show $ numTermChild p)
  (_, False) -> error $ "term too deep " ++ (show $ numTermDepth p)
  otherwise  -> return ()

allocator :: Properties -> TAllocator LoopingDerivation
allocator p = knownLoopingDerivation (uList maxSteps uStep) uPos uSubst
  where
    uList 0 _ = knownNill
    uList i a = union knownNill $ knownConss a $ uList (i - 1) a

    uUnary 0  = knownZ
    uUnary i  = union knownZ $ knownS $ uUnary $ i - 1

    uPos      = uList (numTermDepth p) $ uUnary $ (numTermChild p) - 1

    uVar      = uNat $ bitWidth $ length $ vars p -- Unary probieren
    uSym      = uNat $ bitWidth $ length $ syms p -- Unary probieren

    uTerm     = go $ numTermDepth p
      where
        go 0  = union (knownVar uVar) (knownNode uSym knownNill)
        go i  = union (knownVar uVar) (knownNode uSym $ uList (numTermChild p) $ go $ i - 1)

    uRule     = knownPair uTerm uTerm

    uSubst :: TAllocator (List (Pair Nat Term))
    uSubst    = foldr (\var -> knownConss (knownPair (fromKnown var) uTerm)) knownNill 
                      $ map nat [0 .. toInteger ((length $ vars p) - 1)]

    uStep     = knownStep uTerm uRule uPos uSubst uTerm

type IdentifierMap = M.Map TPDB.Identifier Nat
type InvIdentifierMap = M.Map Nat TPDB.Identifier

makeSymMap :: Properties -> IdentifierMap
makeSymMap p = M.fromList $ zip (syms p) $ map nat [0..]

makeVarMap :: Properties -> IdentifierMap
makeVarMap p = M.fromList $ zip (vars p) $ map nat [0..]

mapTrs :: IdentifierMap -> IdentifierMap -> TPDB.TRS TPDB.Identifier TPDB.Identifier 
       -> List (Pair Term Term)
mapTrs symMap varMap = foldr Conss Nill . map rule . TPDB.rules
  where
    rule r                = Pair (term $ TPDB.lhs r) (term $ TPDB.rhs r)
    term (TPDB.Var v)     = Var $ varMap M.! v
    term (TPDB.Node s ts) = Node (symMap M.! s) $ foldr Conss Nill $ map term ts

prettyVar :: InvIdentifierMap -> Nat -> String
prettyVar invVarMap v = case M.lookup v invVarMap of
  Nothing -> "WHATEVER" --error $ unwords [show v, "is no variable"]
  Just v' -> TPDB.name v'

prettyList :: (a -> String) -> List a -> String
prettyList f xs = concat ["[", go (map' f xs), "]"]
  where
    go Nill = ""
    go (Conss y ys) = case ys of
      Nill -> y
      Conss {} -> concat [y, ", ", go ys]

prettyTerm :: InvIdentifierMap -> InvIdentifierMap -> Term -> String
prettyTerm invSigMap invVarMap term = case term of
  Var v -> "Var " ++ (prettyVar invVarMap v)
  Node f ts -> unwords ["Node" , prettySym f, prettyList (prettyTerm invSigMap invVarMap) ts]

  where
    prettySym s = case M.lookup s invSigMap of
      Nothing -> "WHATEVER" --error $ unwords [show s, "is no symbol"]
      Just s' -> TPDB.name s'

prettyRule :: InvIdentifierMap -> InvIdentifierMap -> Pair Term Term -> String
prettyRule invSigMap invVarMap (Pair l r) = unwords [ prettyTerm invSigMap invVarMap l
                                                    , "->"
                                                    , prettyTerm invSigMap invVarMap r ]

prettyLD :: InvIdentifierMap -> InvIdentifierMap -> LoopingDerivation -> String
prettyLD invSigMap invVarMap (LoopingDerivation steps pos subst) =
  unlines [ "LoopingDerivation"
          , "  " ++ (prettyList prettyStep steps)
          , "  " ++ (prettyList prettyUnary pos)
          , "  " ++ (prettyList prettySubst subst)
          ]
  where
    prettyStep (Step t0 rule pos subst t1) =
      unwords [ "Step"
              , parens $ prettyTerm invSigMap invVarMap t0
              , parens $ prettyRule invSigMap invVarMap rule
              , prettyList prettyUnary pos
              , prettyList prettySubst subst
              , parens $ prettyTerm invSigMap invVarMap t1
              ]

    prettyUnary = show . go
      where go Z     = 0
            go (S u) = 1 + (go u)

    prettySubst (Pair var term) = concat [ "(", prettyVar invVarMap var
                                         , ", ", prettyTerm invSigMap invVarMap term
                                         , ")" ]

    parens x = concat [ "(", x, ")" ]

prettyTRS :: InvIdentifierMap -> InvIdentifierMap -> List (Pair Term Term) -> String
prettyTRS invSigMap invVarMap rules = unlines $ map (prettyRule invSigMap invVarMap) $ go rules
  where
    go Nill         = []
    go (Conss r rs) = r : (go rs)

mainFile :: FilePath -> IO ()
mainFile file = TPDB.get file >>= \case
  Right _ -> error "SRS not supported"
  Left trs -> do --putStrLn $ prettyTRS invSigMap invVarMap trs'
                 checkValidity p
                 solveAndTestP trs' (allocator p) encConstraint constraint >>= \case
                   Nothing -> putStrLn "Nothing"
                   Just ld -> putStrLn $ prettyLD invSigMap invVarMap ld
    where
      p         = properties trs
      symMap    = makeSymMap p
      varMap    = makeVarMap p
      invSigMap = M.fromList $ map swap $ M.toList symMap
      invVarMap = M.fromList $ map swap $ M.toList varMap
      trs'      = mapTrs symMap varMap trs

main :: IO ()
main = getArgs >>= \[file] -> mainFile file
