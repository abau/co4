{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
  (solve)
where

import           Debug.Trace (traceShow)
import qualified Satchmo.SAT.Mini as Backend 
import           Satchmo.Code (Decode,decode)
import           Satchmo.Boolean (assert)
import           CO4.EncodedAdt (EncodedAdt,flag)
import           CO4.Algorithms.Eitherize.DecodedAdtTypeFamily (DecodedAdt)
import           CO4.Algorithms.Eitherize.UnknownGadtInstance (Unknown (unknown))

-- |Main entry point for solving an encoded constraint system
solve :: (Unknown a, Decode Backend.SAT EncodedAdt (DecodedAdt a)) 
      => a -> (EncodedAdt -> Backend.SAT EncodedAdt) -> IO (Maybe (DecodedAdt a))
solve (undef :: a) constraint = 
  Backend.solve $ do u      <- unknown undef
                     result <- traceShow u $ constraint u
                     assert [flag result]
                     return ( ( decode u ) :: Backend.SAT (DecodedAdt a))
