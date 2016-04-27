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

uUnary 0 = knownZ
uUnary i = union knownZ $ knownS $ uUnary $ i-1
matrixAllocator n a = kList n $ kList n a
blockAllocator n = matrixAllocator n $ uUnary $ (n*n) - 1
boardAllocator n = matrixAllocator n $ blockAllocator n

fromUnary :: Unary -> Int
fromUnary u = case u of
  Z -> 0
  S u' -> 1 + (fromUnary u')

result :: Int -> IO ()
result n = do
  result <- solveAndTest (boardAllocator n) encConstraint constraint
  case result of
    Nothing -> putStrLn "none"
    Just b  -> 
      forM_ (rows b $ unaryLength b) $ putStrLn . concatMap (printf "%3i " . fromUnary)

main :: IO ()
main = getArgs >>= result . read . head
