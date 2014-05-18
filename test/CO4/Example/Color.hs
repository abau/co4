{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CO4.Example.Color
where

import           Prelude (undefined,(>>=),error,Show (..),putStrLn,(.),id)
import qualified Data.Maybe as M
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( [d| data Bool       = False | True
       data Color      = Red | Green | Blue deriving Show
       data Monochrome = Black | White deriving Show
       data Pixel      = Colored Color
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
        Colored    c -> case c of
          Blue -> True
          _    -> False

   |] >>= compile []
  )

allocator = complete

result = solveAndTestP True allocator encConstraint constraint
