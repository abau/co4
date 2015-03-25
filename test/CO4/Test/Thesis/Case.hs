{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.Thesis.Case
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.Thesis.CaseStandalone

$( compileFile [ImportPrelude,Profile] "test/CO4/Test/Thesis/CaseStandalone.hs" )

result :: Bool -> IO (Maybe Bool)
result x = solveAndTestP x complete encConstraint constraint
