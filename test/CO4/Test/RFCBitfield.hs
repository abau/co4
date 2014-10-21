{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           System.Environment (getArgs)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.RFCBitfieldStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/RFCBitfieldStandalone.hs" )

allocator :: Int -> Int -> Int -> TAllocator [Grid]
allocator r c col = kList col $ kList r $ kList c complete

toIndex :: Int -> Index
toIndex 1 = This
toIndex n = Next $ toIndex $ n - 1

result :: Int -> Int -> Int -> IO String
result rs cs col = do
  grids <- solveAndTestP (toIndex rs, toIndex cs) (allocator rs cs col) encConstraint constraint
  case grids of
    Nothing -> return "Nothing"
    Just gs -> return $ unlines $ map showGrid gs

showGrid :: Grid -> String
showGrid = unlines . map (map toChar)
  where
    toChar False = '0'
    toChar True  = 'X'

main :: IO ()
main = do
  [rs,cs,col] <- getArgs >>= return . map read
  result rs cs col >>= putStrLn
