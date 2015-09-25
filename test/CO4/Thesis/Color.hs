{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Prelude hiding (Bool(..))
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import           System.Environment (getArgs)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool       = False | True deriving Read
       data Color      = Red | Green | Blue deriving Show
       data Monochrome = Black | White deriving Show
       data Pixel      = Foreground Color
                       | Background Monochrome
                         deriving Show

       constraint :: Bool -> Pixel -> Bool
       constraint p u = case p of
        False -> case u of Background _ -> True
                           _            -> False
        True -> isBlue u

       isBlue :: Pixel -> Bool
       isBlue u = case u of
        Background _ -> False
        Foreground f -> case f of
          Blue -> True
          _    -> False

   |] >>= compile []
  )

main :: IO ()
main = do
  [ p ]  <- getArgs
  result <- solveAndTestP (read p) complete encConstraint constraint
  putStrLn (show result)
