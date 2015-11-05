{-# LANGUAGE LambdaCase #-}
module CO4.CodeGen.Names
  (encodedConsName, encodedName)
where

import Data.Char (toUpper)
import CO4.Names 
  (Namelike,mainName,deprecatedMainName,mapName,toValidDataIdentifier,toValidFunIdentifier)
import CO4.Config (MonadConfig,is,Config(Profile))

encodedConsName :: (Eq a, Namelike a) => a -> a
encodedConsName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns) ++ "Cons") 
                . toValidDataIdentifier

encodedName :: (MonadConfig m, Eq a, Namelike a) => a -> m a
encodedName name = is Profile >>= \case
  False -> return $ mapName (enc . toValidFunIdentifier) name

  True  -> return $ mapName 
             (\case n | n == mainName           -> enc n
                    n | n == deprecatedMainName -> enc n
                    n                           -> enc (toValidFunIdentifier n) ++ "Prof") name
  where
    enc (n:ns) = "enc" ++ (toUpper n : ns)
