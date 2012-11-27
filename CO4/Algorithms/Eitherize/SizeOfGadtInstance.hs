{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.SizeOfGadtInstance
  (SizeOf (..), sizeOf, sizeOfGadtInstances)
where

import           Data.Maybe (catMaybes)
import           Data.List (nub)  
import           Data.Either (rights)
import           Control.Monad (liftM,forM)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Util (isRecursiveAdt)
import           CO4.THUtil 
import           CO4.Unique
import           CO4.Algorithms.Eitherize.Util
  
class SizeOf a where
  numFlags    :: a -> Int
  numDataBits :: a -> Int

sizeOf :: SizeOf a => a -> Int
sizeOf a = numFlags a + numDataBits a

sizeOfGadtInstances :: MonadUnique u => Declaration -> Gadt u [TH.Dec]
sizeOfGadtInstances adt = sequence $
  if isRecursiveAdt adt 
  then [ sizeOfGadtInstance False adt , sizeOfGadtInstance True adt ]
  else [ sizeOfGadtInstance False adt ]

sizeOfGadtInstance :: MonadUnique u => Bool -> Declaration -> Gadt u TH.Dec
sizeOfGadtInstance doRecursions adt = do
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

      declaration1 = funD' "numFlags" [typedWildcard instanceType] 
                   $ intE
                   $ ceiling $ logBase 2 $ fromIntegral 
                   $ length  $ dAdtConstructors adt

  declaration2 <- funD' "numDataBits" [typedWildcard instanceType] 
                  `liftM` sumConstructorSizes doRecursions gadtArgs

  return $ TH.InstanceD (classPredicates constraints)
                        (TH.AppT (TH.ConT ''SizeOf) instanceType)
                        [declaration1, declaration2] 

classPredicates :: [TH.Type] -> [TH.Pred]
classPredicates = map (\t -> TH.ClassP ''SizeOf [t]) . filter isParametrized
  where 
    isParametrized (TH.VarT {})  = True
    isParametrized (TH.AppT a b) = isParametrized a || isParametrized b
    isParametrized _             = False

sumConstructorSizes :: MonadUnique u => Bool -> [Either [TH.Type] [TH.Type]] 
                                                -> u TH.Exp
sumConstructorSizes doRecursions argss = do
  (sumNames, bindings) <- (unzip . catMaybes) `liftM` mapM mkBindings sizeOfArgss

  let maxOfSums = TH.AppE (TH.VarE 'maximum) $ TH.ListE $ map varE sumNames

  return $ letE' (concat bindings) maxOfSums
  
  where 
    mkBindings :: MonadUnique u => Maybe [TH.Exp] -> u (Maybe (Name, [(Name,TH.Exp)]))
    mkBindings Nothing     = return Nothing
    mkBindings (Just exps) = do
      sizeNames <- forM exps $ const $ newName ""
      sumName   <- newName ""

      let sizeBindings = zip sizeNames exps
          sumBinding   = (sumName, TH.AppE (TH.VarE 'sum)
                                         $ TH.ListE $ map varE sizeNames)

      return $ Just (sumName, sizeBindings ++ [sumBinding])

    sizeOfArgss :: [ Maybe [TH.Exp] ]
    sizeOfArgss = map sizeOfArgs_ argss

    sizeOfArgs_ :: Either [TH.Type] [TH.Type] -> Maybe [TH.Exp]
    sizeOfArgs_ (Left  args) | doRecursions = sizeOfArgs_ $ Right args
    sizeOfArgs_ (Left  _)    | otherwise    = Nothing
    sizeOfArgs_ (Right args)                = Just $ map sizeOfArg args

    sizeOfArg :: TH.Type -> TH.Exp
    sizeOfArg = TH.AppE (TH.VarE 'sizeOf) . typedUndefined
