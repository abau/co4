{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Algorithms.Eitherize.Solve
  (solve)
where

-- import           Debug.Trace (traceShow)
import qualified Satchmo.SAT.Mini as Backend 
import           Satchmo.Code (Decode,decode)
import           Satchmo.Boolean (assert)
import           CO4.EncodedAdt (EncodedAdt,unknown,flags)
import           CO4.Algorithms.Eitherize.UnsizedAdt (UnsizedAdt)
import           CO4.Algorithms.Eitherize.IndexedGadt (Indexed (index))

-- |Main entry point for solving an encoded constraint system
solve :: (Indexed a, Decode Backend.SAT EncodedAdt (UnsizedAdt a)) 
      => a -> (EncodedAdt -> Backend.SAT EncodedAdt) -> IO (Maybe (UnsizedAdt a))
solve (undef :: a) constraint = 
  Backend.solve $ do u      <- unknown $ index 0 undef
                     result <- {-traceShow u $-} constraint u
                     assert [ head $ flags result ]
                     return ( ( decode u ) :: Backend.SAT (UnsizedAdt a))
