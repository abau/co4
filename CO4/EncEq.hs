{-# LANGUAGE MultiParamTypeClasses #-}
module CO4.EncEq 
  (EncEq (..), EncProfiledEq (..), encEq, encProfiledEq)
where

import Satchmo.Core.MonadSAT (MonadSAT)
import Satchmo.Core.Primitive (Primitive)
import CO4.EncodedAdt (EncodedAdt (make))
import CO4.Profiling (MonadProfiling)

class (Primitive p, EncodedAdt e p) => EncEq a e p where
  encEqPrimitive :: (MonadSAT m) => a -> e p -> e p -> m p

class (Primitive p, EncodedAdt e p) => EncProfiledEq a e p where
  encProfiledEqPrimitive :: (MonadSAT m, MonadProfiling m) 
                         => a -> e p -> e p -> m p

encEq :: (MonadSAT m, EncodedAdt e p, EncEq a e p) => a -> e p -> e p -> m (e p)
encEq a b c = encEqPrimitive a b c >>= \p -> return $ make [p]

encProfiledEq :: (MonadSAT m, MonadProfiling m, EncodedAdt e p, EncProfiledEq a e p) 
              => a -> e p -> e p -> m (e p)
encProfiledEq a b c = encProfiledEqPrimitive a b c >>= \p -> return $ make [p]
