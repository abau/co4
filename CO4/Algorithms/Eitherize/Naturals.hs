{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
-- |Type level naturals
module CO4.Algorithms.Eitherize.Naturals
where

import           Control.Monad (forM)
import qualified Language.Haskell.TH as TH

data Nat0
data NatSucc a

$( forM [1..100] $ \i -> 
    let nat i = TH.mkName $ "Nat" ++ (show i)
    in
      TH.tySynD (nat i) [] (TH.appT (TH.conT ''NatSucc) 
                           (TH.conT $ nat $ i - 1))
 )

