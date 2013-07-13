{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude

$( [d| constraint p u = case u of
           Left  xs -> p == and (assertKnown xs)
           Right ys -> p == or ys

   |] >>= runIO . configurable [Verbose, ImportPrelude, Profile] . compile 
  )

main = do
    solveAndTestP 
         True
         ( constructors [ Just [ kList 2 uBool ], Just [ kList 2 uBool ] ] )
         encConstraint constraint

