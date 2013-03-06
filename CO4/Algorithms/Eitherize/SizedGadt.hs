{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.SizedGadt
  (Nat0, NatSucc, sizedGadts)
where

import           Control.Monad (liftM)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Unique
import           CO4.Util 
import           CO4.THUtil 
import           CO4.TypesUtil (countTCon) 
import           CO4.Names (convertName)
import           CO4.Algorithms.TopologicalSort (adtGroups)
import           CO4.Algorithms.Eitherize.Util
import           CO4.Algorithms.Eitherize.UnsizedAdtInstance (unsizedAdtInstance)
import           CO4.Algorithms.Eitherize.IndexedInstance (indexedInstances)

sizedGadts :: MonadUnique u => [Declaration] -> u [TH.Dec]
sizedGadts adts = 
  let adtGroups_        = adtGroups adts
      adts'             = concat adtGroups_
      assertAllOne [] b = b
      assertAllOne (a:as) b =
        if lengthOne a then assertAllOne as b
        else error $ "Mutually recursive ADTs are not supported: " ++ show (map dAdtName a)

      mkSizedGadts     _      []     = return []
      mkSizedGadts sizedTypes (adt:adts) = do
        (decs,adtName,adtArity) <- sizedGadt sizedTypes adt
        decss <- mkSizedGadts (addToSizedTypes adtName adtArity sizedTypes) adts
        return $ decs ++ decss
  in
    assertAllOne adtGroups_ $ mkSizedGadts emptySizedTypes adts'

sizedGadt :: MonadUnique u => SizedTypes -> Declaration -> u ([TH.Dec],UntypedName,Int)
sizedGadt sizedTypes adt = runGadt sizedTypes adt sizedGadt'
  where
    adtName    = dAdtName adt
    adtConss   = dAdtConstructors adt
    sizedGadt' = do
      gadtConss     <- mapM (sizedGadtConstructor adtName) adtConss 
      allSizeParams <- getAllSizeParameters

      let gadtVars = allSizeParams ++ (dAdtTypeVariables adt)
          gadt     = TH.DataD [] (sizedName $ convertName adtName)
                                 (map (TH.PlainTV . convertName) gadtVars )
                                 gadtConss
                                 []

      indexedInstance <- indexedInstances adt
      unsizedInstance <- unsizedAdtInstance adt
      return ( gadt : unsizedInstance : indexedInstance, adtName, length allSizeParams)

sizedGadtConstructor :: MonadUnique u => UntypedName -> Constructor -> Gadt u TH.Con
sizedGadtConstructor adtName (CCon consName consArgs) = 
  if isRecursiveConstructor
  then recursiveGadtConstructor
  else nonRecursiveGadtConstructor

  where 
    isRecursiveConstructor = sum (map (countTCon adtName) consArgs) > 0

    recursiveGadtConstructor = do
      Just recSizeParameter <- getRecursiveSizeParameter
      recSizeParameter'     <- newNamelike recSizeParameter
      consArgs'             <- mapM (gadtType $ Just recSizeParameter') consArgs

      return $ TH.ForallC [ TH.PlainTV $ convertName recSizeParameter' ]
                          [ TH.EqualP (varT recSizeParameter)
                                      (TH.AppT (TH.ConT ''NatSucc) 
                                               (varT recSizeParameter'))
                          ]
             $ normalC' (sizedName consName) consArgs'

    nonRecursiveGadtConstructor = do
      predicates <- do
        mRecSizeParam <- getRecursiveSizeParameter
        case mRecSizeParam of
          Nothing -> return []
          Just p  -> return [ TH.EqualP (varT p) (TH.ConT ''Nat0) ]

      consArgs' <- mapM (gadtType Nothing) consArgs

      return $ TH.ForallC [] predicates $ normalC' (sizedName consName) consArgs'

    gadtType mLocalRecSizeParameter type_ = case type_ of
      TVar v -> return $ varT v
      TCon c ts | c == adtName -> do
        let recParameter = maybe [] (\p -> [TH.VarT p]) mLocalRecSizeParameter
        sizeParams <- liftM (map varT) $ getSizeParameters
        ts' <- mapM (gadtType mLocalRecSizeParameter) ts
        return $ appsT (conT $ sizedName c) $ recParameter ++ sizeParams ++ ts'

      TCon c ts | otherwise -> do
        sizeParams <- liftM (map varT) $ nextConstructorSizeParameters c
        ts' <- mapM (gadtType mLocalRecSizeParameter) ts
        return $ appsT (conT $ sizedName c) $ sizeParams ++ ts'
