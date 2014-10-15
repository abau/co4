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
import           CO4.Test.RFCStandaloneBitfield

$( compileFile [Cache,ImportPrelude] "test/CO4/Test/RFCStandaloneBitfield.hs" )

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
  [rs,cs,col] <- getArgs
  result (read rs) (read cs) (read col) >>= putStrLn
