{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module CO4.Algorithms.Eitherize.DecodedAdtTypeFamily
  (DecodedAdt, decodedAdtTypeInstance)
where

import           Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.THUtil
import           CO4.Algorithms.Eitherize.Util

type family DecodedAdt a :: *

-- |Builds an instance of the @DecodedAdt@ type family:
--
-- @type instance DecodedAdt (SizedFoo <size-args> <args>) = Foo <args>@
decodedAdtTypeInstance :: (Monad m,Functor m) => Declaration -> Gadt m TH.Dec
decodedAdtTypeInstance (DAdt adtName adtVars _) = do
  recSizeParam <- maybe [] (\p -> [varT p]) <$> getRecursiveSizeParameter
  sizeParams   <- map varT <$> getSizeParameters

  return $ TH.TySynInstD ''DecodedAdt [ appsT (conT $ sizedName adtName)
                                          $  recSizeParam
                                          ++ sizeParams
                                          ++ (map varT adtVars) ]
         $ appsT (conT adtName) 
         $ map (TH.AppT (conT ''DecodedAdt) . varT) adtVars
