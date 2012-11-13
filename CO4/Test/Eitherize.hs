{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module CO4.Test.Eitherize
where

import           Prelude hiding (Bool(..),and,not,(&&),(||))
import           Control.Applicative ((<$>))
import qualified Language.Haskell.TH as TH
import           Satchmo.Boolean (assert)
import           Satchmo.Code (Decode,decode)
import           Satchmo.SAT.Mini (SAT,solve)
import           CO4
import           CO4.EncodedAdt

$([d|
      data Boolean = True   | False 
      data Fuzzy   = Fuzzy  | NotFuzzy Boolean
      data Param   = Param1 | Param2 

      not a = case a of True -> False ; False -> True

      and x y = case x of True  -> case y of True  -> True
                                             False -> False
                          False -> False

      fuzzyAnd x y = case x of 
        Fuzzy       -> Fuzzy
        NotFuzzy x' -> case y of Fuzzy       -> Fuzzy
                                 NotFuzzy y' -> NotFuzzy (and x' y')
                  
      isFuzzy x = case x of Fuzzy      -> True
                            NotFuzzy _ -> False

      main p u v = case p of Param1 ->      isFuzzy (fuzzyAnd u v)
                             Param2 -> not (isFuzzy (fuzzyAnd u v))

   |] >>= \p -> TH.runIO $ compile p [ Verbose, NoRaml --,    NoSatchmo
                                     , DumpAll ""]
  )

invMain :: Param -> IO ()
invMain param = do
  mResult <- solve ( do u <- unknownAdt (undefined :: Fuzzy) 2
                        v <- unknownAdt (undefined :: Fuzzy) 3
                        a <- encMain param u v
                        assert [flag a]
                        return ( decode [u,v] )
                 ) :: IO (Maybe [Fuzzy])
  putStrLn $ show mResult

deriving instance Show Fuzzy
deriving instance Show Boolean
