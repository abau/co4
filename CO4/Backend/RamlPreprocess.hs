{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Backend.RamlPreprocess
  (preprocess)
where

import           CO4.Language
import           CO4.Names 
import           CO4.Unique
import           CO4.Backend.TH (isTupleCon)
import           CO4.Algorithms.Instantiator
import qualified CO4.Algorithms.HindleyMilner as HM

newtype Preprocessor a = Preprocessor { runPP :: Unique a }
  deriving (Functor, Monad)

instance MonadInstantiator Preprocessor where

  -- Generate nonsense Nil match
  instantiateCase (ECase e [Match (PCon c [PVar x, PVar xs]) e1])
      | c == consCon = do

    SType e1Type <- Preprocessor $ HM.schemeOfExp HM.prelude e1

    return $ ECase e 
      [ Match (PCon nilCon []) (nil e1Type)
      , Match (PCon c [PVar x, PVar xs]) e1
      ]
  
    where 
      nil (TCon c [])  | c == intType    = ELit $ LInt 0
      nil (TCon c [])  | c == charType   = ELit $ LChar '0'
      nil (TCon c [])  | c == doubleType = ELit $ LDouble 0
      nil (TCon c [])  | c == boolType   = ECon trueCon
      nil (TCon c [a]) | c == listType   = EApp (ECon consCon) [nil a, ECon nilCon]
      nil (TCon c as)  | isTupleCon c    = EApp (ECon $ tupleCon $ length as)
                                             $ map nil as
      
  instantiateCase (ECase e ms) = do
    e'  <- instantiateExpression e
    ms' <- mapM instantiateMatch ms
    return $ ECase e' ms'

preprocess :: Program -> Unique Program
preprocess = runPP . instantiateProgram
