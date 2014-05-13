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

uList 0 _  = unsafeTAllocator $ constructors [ M.Just [] , M.Nothing ]
uList i a  = unsafeTAllocator $ constructors [ M.Just [] , M.Just [ toAllocator a
                                                                  , toAllocator $ uList (i-1) a ] ]

uName = completeName

uTerm 0 = unsafeTAllocator $ constructors [ M.Just [ toAllocator uName ]
                                          , M.Nothing
                                          , M.Just []
                                          , M.Just []
                                          , M.Just []
                                          ]

uTerm depth = unsafeTAllocator $ constructors 
  [ M.Just [ toAllocator uName ]
  , M.Just [ toAllocator $ uTerm (depth - 1)
           , toAllocator $ uTerm (depth - 1)
           , toAllocator $ uTerm (depth - 1) ]
  , M.Just []
  , M.Just []
  , M.Just []
  ]

uRule termDepth = knownRule (uList 2 uName)
                            (uTerm termDepth)
                            (uTerm termDepth)

uStep termDepth numSubst = knownStep (uTerm termDepth)
                                     (uRule termDepth)
                                     (uPosition termDepth)
                                     (uSubstitution numSubst termDepth)
                                     (uTerm termDepth)

uPos = completePos
uPosition l               = uList l uPos
uSubstitution l termDepth = uList l (knownPair uName (uTerm termDepth))

uDerivation numSteps termDepth numSubst = uList numSteps (uStep termDepth numSubst)

uLoopingDerivation numSteps numSubst termDepth = 
  knownLooping_Derivation ( uDerivation numSteps termDepth numSubst )
                          ( uPosition termDepth )
                          ( uSubstitution numSubst termDepth )

result = solveAndTest (uLoopingDerivation 3 2 2) encConstraint constraint

main = result
