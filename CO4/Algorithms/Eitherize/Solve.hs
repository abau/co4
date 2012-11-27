{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
  (solve)
where

import           Prelude hiding (not)
import           Debug.Trace (traceShow)
import qualified Satchmo.SAT.Mini as Backend 
import           Satchmo.Code (Decode,decode)
import           Satchmo.Boolean (assert,not)
import           CO4.EncodedAdt (EncodedAdt,unknown,flags)
import           CO4.Algorithms.Eitherize.DecodedAdtTypeFamily (DecodedAdt)
--import           CO4.Algorithms.Eitherize.UnknownGadtInstance (Unknown (unknown))
import           CO4.Algorithms.Eitherize.IndexedGadt (Indexed (index))

-- |Main entry point for solving an encoded constraint system
solve :: (Indexed a, Decode Backend.SAT EncodedAdt (DecodedAdt a)) 
      => a -> (EncodedAdt -> Backend.SAT EncodedAdt) -> IO (Maybe (DecodedAdt a))
solve (undef :: a) constraint = 
  Backend.solve $ do u      <- unknown $ index 0 undef
                     result <- traceShow u $ constraint u
                     traceShow result $ assert [not $ head $ flags result]
                     return ( ( decode u ) :: Backend.SAT (DecodedAdt a))
