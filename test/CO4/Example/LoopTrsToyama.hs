{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CO4.Example.LoopTrsToyama
where

import qualified Prelude ; import Prelude (($), (-), (*))

import qualified Data.Maybe as M

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Example.LoopTrsToyamaStandalone

$( compileFile [] "test/CO4/Example/LoopTrsToyamaStandalone.hs" )

uList 0 _  = knownNil
uList i a  = union knownNil $ knownCons a $ uList (i-1) a

uName = complete

uTerm 0 = unions [ knownV uName, knownA, knownB, knownC ]

uTerm depth = union (uTerm 0) $ knownF (uTerm $ depth - 1)
                                       (uTerm $ depth - 1)
                                       (uTerm $ depth - 1)

uRule termDepth = knownRule (uList 2 uName)
                            (uTerm termDepth)
                            (uTerm termDepth)

uStep termDepth numSubst = knownStep (uTerm termDepth)
                                     (uRule termDepth)
                                     (uPosition termDepth)
                                     (uSubstitution numSubst termDepth)
                                     (uTerm termDepth)

uPos = complete
uPosition l               = uList l uPos
uSubstitution l termDepth = uList l (knownPair uName (uTerm termDepth))

uDerivation numSteps termDepth numSubst = uList numSteps (uStep termDepth numSubst)

uLoopingDerivation numSteps numSubst termDepth = 
  knownLooping_Derivation ( uDerivation numSteps termDepth numSubst )
                          ( uPosition termDepth )
                          ( uSubstitution numSubst termDepth )

result = solveAndTest (uLoopingDerivation 3 2 2) encConstraint constraint

main = result
