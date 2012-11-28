{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module CO4.Algorithms.Eitherize.IndexedGadtInstance
where

import           Data.List (nub)  
import           Data.Either (rights)
import           Control.Monad (liftM)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Util (isRecursiveAdt)
import           CO4.THUtil 
import           CO4.Unique
import           CO4.Algorithms.Eitherize.Util
import           CO4.Algorithms.Eitherize.IndexedGadt

indexedGadtInstances :: MonadUnique u => Declaration -> Gadt u [TH.Dec]
indexedGadtInstances adt = sequence $
  if isRecursiveAdt adt 
  then [ indexedGadtInstance False adt , indexedGadtInstance True adt ]
  else [ indexedGadtInstance False adt ]

indexedGadtInstance :: MonadUnique u => Bool -> Declaration -> Gadt u TH.Dec
indexedGadtInstance doRecursions adt = do
  resetSizeArgumentCounter 
  gadtArgs      <- gadtConstructorArgs adt
  sizeParams    <- getSizeParameters
  mRecSizeParam <- getRecursiveSizeParameter

  let instanceRecSizeParam = case (mRecSizeParam,doRecursions) of 
          (Nothing,False) -> []
          (Just _ ,False) -> [ TH.ConT ''Nat0 ]
          (Just p ,True ) -> [ TH.AppT (TH.ConT ''NatSucc) (varT p) ]

      gadtName    = sizedName $ dAdtName adt
      adtVars'    = map varT $ dAdtTypeVariables adt
      sizeParams' = map varT   sizeParams

      instanceType = appsT (conT gadtName) $ instanceRecSizeParam ++ sizeParams' ++ adtVars' 


      constraints = nub $ if doRecursions 
                    then (concat $ map (either id id) gadtArgs) ++ adtVars'
                    else (concat $ rights             gadtArgs) ++ adtVars'

  indexFrom   <- newName ""
  declaration <- funD' "index" [varP indexFrom, typedWildcard instanceType] 
                  `liftM` indexExp doRecursions indexFrom gadtArgs

  return $ TH.InstanceD (classPredicates constraints)
                        (TH.AppT (TH.ConT ''Indexed) instanceType)
                        [declaration] 

classPredicates :: [TH.Type] -> [TH.Pred]
classPredicates = map (\t -> TH.ClassP ''Indexed [t]) . filter isParametrized
  where 
    isParametrized (TH.VarT {})  = True
    isParametrized (TH.AppT a b) = isParametrized a || isParametrized b
    isParametrized _             = False

indexExp :: MonadUnique u => Bool -> Name -> [Either [TH.Type] [TH.Type]] -> u TH.Exp
indexExp doRecursions flagFrom argss = 
  return $ appsE (TH.VarE 'indexedGadt) $ [ varE flagFrom 
                                          , TH.ListE $ map indexed argss
                                          ]
  where
    indexed args = case args of
      Right as -> TH.AppE (TH.ConE 'Just) $ TH.ListE $ map wrap as

      Left  as | doRecursions -> indexed $ Right as
      Left  _  | otherwise    -> TH.ConE 'Nothing

    wrap = TH.AppE (TH.ConE 'IndexedWrapper) . typedUndefined
