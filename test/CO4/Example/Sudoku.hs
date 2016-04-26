{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import           Language.Haskell.TH (runIO)
import           Control.Monad (forM_)
import           Text.Printf (printf)
import           System.Environment (getArgs)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Example.SudokuStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Example/SudokuStandalone.hs" )

natWidth n = ceiling $ log (fromIntegral $ n * n) / log 2
matrixAllocator n a = kList n $ kList n a
blockAllocator n = matrixAllocator n $ uNat $ natWidth n
boardAllocator n = matrixAllocator n $ blockAllocator n

result :: Int -> IO ()
result n = do
  let param = nat (natWidth n) $ fromIntegral $ (n * n) - 1
  result <- solveAndTestP param (boardAllocator n) encConstraint constraint
  case result of
    Nothing -> putStrLn "none"
    Just b  -> 
      forM_ (rows b $ unaryLength b) $ putStrLn . concatMap (printf "%3i " . value)

main :: IO ()
main = getArgs >>= result . read . head
