{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
module CO4.Test.RFC
where
-}

import           System.Environment (getArgs)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.RFCStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/RFCStandalone.hs" )

allocator r c col = kList r $ kList c $ uNat col

toIndex :: Int -> Index
toIndex 1 = This
toIndex n = Next $ toIndex $ n - 1

result :: Int -> Int -> Int -> IO String
result rs cs col = do
  grid <- solveAndTestP (toIndex rs, toIndex cs) (allocator rs cs col) encConstraint constraint
  case grid of
    Nothing -> return "Nothing"
    Just g  -> return $ unlines $ map show g

main :: IO ()
main = do
  [rs,cs,col] <- getArgs
  result (read rs) (read cs) (read col) >>= putStrLn
