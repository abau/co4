{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.UnsizedAdtInstance
  (UnsizedAdt, unsizedAdtInstance)
where

import           Control.Monad (liftM)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.THUtil
import           CO4.Algorithms.Eitherize.Util
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)

-- |Builds an instance of the @UnsizedAdt@ type family:
--
-- @type instance UnsizedAdt (SizedFoo <size-args> <args>) = Foo <args>@
unsizedAdtInstance :: (Monad m) => Declaration -> Gadt m TH.Dec
unsizedAdtInstance (DAdt adtName adtVars _) = do
  recSizeParam <- liftM (maybe [] $ \p -> [varT p]) $ getRecursiveSizeParameter
  sizeParams   <- liftM (map varT) getSizeParameters

  return $ TH.TySynInstD ''UnsizedAdt [ appsT (conT $ sizedName adtName)
                                          $  recSizeParam
                                          ++ sizeParams
                                          ++ (map varT adtVars) ]
         $ appsT (conT adtName) 
         $ map (TH.AppT (conT ''UnsizedAdt) . varT) adtVars
