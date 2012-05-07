{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module CO4.MonadifyTypes
where

import qualified Prelude
import           Prelude (Bool,return,(.))
import           Control.Monad.Identity (Monad,Identity)
import qualified Satchmo.Boolean as Boolean
import           Satchmo.Boolean (Boolean)
import           Satchmo.SAT.Mini (SAT)

class Constant a where  
     constant :: Prelude.Bool -> a

instance Constant Boolean where
     constant = Boolean.Constant

instance Constant Bool where     
     constant = Prelude.id
                    
false = constant Prelude.False                
true  = constant Prelude.True

class Monad m => And m a b c | a b -> c , c -> a , c -> b where
    and :: a -> b -> m c
    
instance And SAT Boolean Boolean Boolean where
    x `and` y = Boolean.and [x,y]

instance And Identity Bool Bool Bool where
    x `and` y = return ( x Prelude.&& y )

class Monad m => Or m a b c | a b -> c , c -> a , c -> b where
    or :: a -> b -> m c
    
instance Or SAT Boolean Boolean Boolean where
    x `or` y = Boolean.or [x,y]

instance Or Identity Bool Bool Bool where
    x `or` y = return ( x Prelude.|| y )

class Monad m => Not m a b | a -> b, b -> a where
    not :: a -> m b
    
instance Not SAT Boolean Boolean where
    not = return . Boolean.not

instance Not Identity Bool Bool where
    not = return . Prelude.not

