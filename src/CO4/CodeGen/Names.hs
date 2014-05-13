{-# LANGUAGE LambdaCase #-}
module CO4.CodeGen.Names
  (encodedConsName, encodedName)
where

import Data.Char (toUpper)
import CO4.Names 
import CO4.Config (MonadConfig,is,Config(Profile))

encodedConsName :: Namelike a => a -> a
encodedConsName = mapName (\(n:ns) -> "enc" ++ (toUpper n : ns) ++ "Cons") 

encodedName :: (MonadConfig m, Namelike a) => a -> m a
encodedName name = is Profile >>= \case
  False -> return $ mapName (\case "&&"   -> "encAnd2"
                                   "||"   -> "encOr2"
                                   n      -> enc n) name

  True  -> return $ mapName (\case n | n == mainName           -> enc n
                                   n | n == deprecatedMainName -> enc n
                                   "&&"                        -> "encAnd2Prof"
                                   "||"                        -> "encOr2Prof"
                                   n                           -> enc n ++ "Prof") name
  where
    enc (n:ns) = "enc" ++ (toUpper n : ns)
