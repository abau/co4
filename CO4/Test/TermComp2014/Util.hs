{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Util
where

import           Data.Char (ord)
import           Data.List (nub)
import qualified Data.Map as M
import           Unsafe.Coerce
import qualified TPDB.Data as TPDB
import qualified TPDB.Plain.Read as Read
import           CO4.Util (toBinary)
import           CO4.Test.TermComp2014.Data
import           CO4.Test.TermComp2014.SL.Standalone (valueOfVar,valueOfTerm)

{-
import TPDB.Plain.Write ()
import TPDB.Pretty (pretty)
import Debug.Trace
-}

parseTrs :: FilePath -> IO Trs
parseTrs path = readFile path >>= return . Read.trs >>= \case
  Left msg  ->    error msg
  Right trs -> do {-putStrLn (show $ pretty trs)-}
                  return $ goTrs trs
  where
    goTrs :: TPDB.TRS TPDB.Identifier TPDB.Identifier -> Trs
    goTrs                     = Trs . map goRule . TPDB.rules
    goRule rule               = Rule (goTerm $ TPDB.lhs rule) (goTerm $ TPDB.rhs rule)
    goTerm (TPDB.Var v)       = Var $ goIdentifier v
    goTerm (TPDB.Node v args) = Node (goIdentifier v) $ map goTerm args

    goIdentifier i | length (TPDB.name i) == 1 = charToSymbol $ head $ TPDB.name i

charToSymbol :: Char -> Symbol
charToSymbol = toBinary Nothing . ord

assignments :: Int -> Trs -> Assignments
assignments n trs = do 
  values <- sequence $ replicate (length vars) [0..(2^n)-1]
  return $ zipWith goMapping vars values
  where
    vars                 = goTrs trs
    goTrs (Trs rules)    = nub $ concatMap goRule rules
    goRule (Rule l r)    = goTerm l ++ (goTerm r)
    goTerm (Var v)       = [v]
    goTerm (Node _ args) = concatMap goTerm args

    goMapping v value = (v, toBinary (Just n) value)

nodeSymbols :: Trs -> [Symbol]
nodeSymbols (Trs rules) = nub $ concatMap goRule rules
  where
    goRule (Rule l r)    = goTerm l ++ (goTerm r)
    goTerm (Var _)       = []
    goTerm (Node f args) = f : (concatMap goTerm args)

fromNat :: Nat -> Int
fromNat Zero     = 0
fromNat (Succ x) = 1 + (fromNat x)

toLabeledTrs :: Int -> Trs -> Model -> (Trs, M.Map Symbol (Symbol, Domain))
toLabeledTrs n (Trs rules) model = (Trs rules', makeMap $ concat mapping)
  where
    (rules',mapping) = unzip $ do r <- rules
                                  s <- sigmas
                                  return $ goRule r s

    sigmas = assignments n $ Trs rules

    makeMap = M.fromListWith (\k v -> error "toLabeledTrs") . nub

    goRule (Rule l r) sigma = 
      let (l', ml) = goTerm l sigma
          (r', mr) = goTerm r sigma
      in
        (Rule l' r', ml ++ mr)

    goTerm (Var v) sigma = (Var v', [(v', (v,l))])
      where 
        v' = v ++ l
        l  = unsafeCoerce $ valueOfVar (unsafeCoerce v) (unsafeCoerce sigma)

    goTerm (Node s args) sigma = (Node s' args', (s',(s,l)) : (concat argMs))
      where 
        s' = s ++ l
      
        l = unsafeCoerce $ valueOfTerm (unsafeCoerce model) (unsafeCoerce sigma)
                             $ unsafeCoerce
                             $ Node s args

        (args', argMs) = unzip $ map (flip goTerm sigma) args
