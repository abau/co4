{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.UnknownGadtInstance
  (Unknown (unknown), unknownGadtInstances)
where

import           Satchmo.SAT.Mini (SAT)
import           Control.Monad.Writer
import           Data.List (nub)
import           Data.Either (rights)
import           Data.Maybe (catMaybes)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Unique
import           CO4.EncodedAdt (EncodedAdt,unknownConstructor,encArgs)
import           CO4.Algorithms.Eitherize.Util
import           CO4.THUtil
import           CO4.Util (isRecursiveAdt)

class Unknown a where
  unknown :: a -> SAT EncodedAdt

-- |Builds an instance of the @Unknown@ class for an ADT.
-- There are two cases:
-- 
-- @Foo@ is a non-recursive ADT.
--
-- > instance Unknown (SizedFoo Nat0 <size-parameters> <type-parameters>) where
-- >  unknown (_ :: SizedFoo Nat0 <size-parameters> <type-parameters>) =
-- >    _11 <- unknown (undefined :: <type of 1st  argument of 1st constructor of foo>)
-- >    _12 <- unknown (undefined :: <type of 2snd argument of 1st constructor of foo>)
-- >    ...
-- >    unknownConstructor [ Just $ encArgs [_11,_12,...], ... ]
--
-- @Foo@ is a recursive ADT. Two @Unknown@ instances are built in this case:
-- an instance for @Foo@ of size @Nat0@ (c.f. the non-recursive case) and an
-- instance for @Foo@ of size @NatSucc n@.
unknownGadtInstances :: MonadUnique u => Declaration -> Gadt u [TH.Dec]
unknownGadtInstances adt = sequence $
  if isRecursiveAdt adt 
  then [ unknownGadtInstance False adt , unknownGadtInstance True adt ]
  else [ unknownGadtInstance False adt ]

unknownGadtInstance :: MonadUnique u => Bool -> Declaration -> Gadt u TH.Dec
unknownGadtInstance doRecursions adt = do
  resetSizeArgumentCounter 
  gadtArgs      <- gadtConstructorArgs adt
  exp           <- unknownConstructorCall doRecursions gadtArgs
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

      declaration = funD' "unknown" [typedWildcard instanceType] exp

      constraints = nub $ if doRecursions 
                    then (concat $ map (either id id) gadtArgs) ++ adtVars'
                    else (concat $ rights             gadtArgs) ++ adtVars'

  return $ TH.InstanceD (classPredicates constraints)
                        (TH.AppT (TH.ConT ''Unknown) instanceType)
                        [declaration] 

-- |Builds a list of @Unknown <type>@ predicates
classPredicates :: [TH.Type] -> [TH.Pred]
classPredicates = map (\t -> TH.ClassP ''Unknown [t]) . filter isParametrized
  where 
    isParametrized (TH.VarT {})  = True
    isParametrized (TH.AppT a b) = isParametrized a || isParametrized b
    isParametrized _             = False

-- |Builds the body of the @unknown@ function
unknownConstructorCall :: MonadUnique u => Bool -> [Either [TH.Type] [TH.Type]] 
                                                -> u TH.Exp
unknownConstructorCall doRecursions argss = do
  bindings <- mapM mkBindings boundExpss

  let thBindings = map (uncurry bindS') $ concat $ catMaybes bindings
      returnStmt = TH.NoBindS $ TH.AppE (TH.VarE 'unknownConstructor) 
                              $ TH.ListE $ map unknownConstructorArguments bindings

  return $ TH.DoE $ thBindings ++ [returnStmt]
  
  where 
    mkBindings Nothing     = return Nothing
    mkBindings (Just exps) = do
      names <- forM exps $ const $ newName ""
      return $ Just $ zip names exps

    boundExpss = map boundExps argss

    boundExps (Left  args) | doRecursions = boundExps $ Right args
    boundExps (Left  _)    | otherwise    = Nothing
    boundExps (Right args)                = Just $ map boundExp args

    boundExp = TH.AppE (TH.VarE 'unknown) . typedUndefined

    unknownConstructorArguments Nothing     = TH.ConE 'Nothing
    unknownConstructorArguments (Just bindings) =
      TH.AppE (TH.ConE 'Just) 
        $ TH.AppE (TH.VarE 'encArgs) 
        $ TH.ListE 
        $ map (varE . fst) bindings
