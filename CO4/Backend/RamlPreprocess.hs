{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CO4.Backend.RamlPreprocess
  (preprocess)
where

import           CO4.Language
import           CO4.Names 
import           CO4.Unique
import           CO4.Algorithms.Instantiator
import           CO4.Algorithms.HindleyMilner (schemeOfExp)
import qualified CO4.HsPrelude as Hs

newtype Preprocessor a = Preprocessor { runPP :: Unique a }
  deriving (Functor, Monad)

instance MonadInstantiator Preprocessor where

  -- Generate nonsense Nil match
  instantiateCase (ECase e [Match (PCon c [PVar x, PVar xs]) e1])
      | c == consName = do

    SType e1Type <- Preprocessor $ schemeOfExp preludeContext e1

    return $ ECase e 
      [ Match (PCon nilName []) (nil e1Type)
      , Match (PCon c [PVar x, PVar xs]) e1
      ]
  
    where 
      nil (TCon c [])  | c == Hs.intName    = ELit $ LInt 0
      nil (TCon c [])  | c == Hs.charName   = ELit $ LChar '0'
      nil (TCon c [])  | c == Hs.doubleName = ELit $ LDouble 0
      nil (TCon c [])  | c == Hs.boolName   = ECon Hs.trueName
      nil (TCon c [a]) | c == Hs.listName   = EApp (ECon Hs.consName) 
                                              [nil a, ECon nilName]
      nil (TCon c as)  | isTupleName c   = EApp (ECon $ tupleName $ length as)
                                             $ map nil as
      
  instantiateCase (ECase e ms) = do
    e'  <- instantiateExpression e
    ms' <- mapM instantiateMatch ms
    return $ ECase e' ms'

preprocess :: Program -> Unique Program
preprocess = runPP . instantiateProgram
