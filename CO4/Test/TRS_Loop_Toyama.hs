{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Prelude ; import Prelude (($), (-), (*))

import qualified Data.Maybe as M

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( compileFile [] "CO4/Test/TRS_Loop_Toyama.standalone.hs" )

uBool      = constructors [ M.Just [] , M.Just [] ]
uList 0 _  = constructors [ M.Just [] , M.Nothing ]
uList i a  = constructors [ M.Just [] , M.Just [a, uList (i-1) a ] ]
uPair a b  = constructors [ M.Just [a,b] ]

uName = constructors [ M.Just [], M.Just [] ]

uTerm 0 = constructors [ M.Just [ uName ]
                       , M.Nothing
                       , M.Just []
                       , M.Just []
                       , M.Just []
                       ]

uTerm depth = constructors 
  [ M.Just [ uName ]
  , M.Just [ uTerm (depth - 1), uTerm (depth - 1), uTerm (depth - 1) ]
  , M.Just []
  , M.Just []
  , M.Just []
  ]

uRule termDepth = constructors [ M.Just [ uList 2 uName
                                        , uTerm termDepth
                                        , uTerm termDepth
                                        ]
                               ]

uStep termDepth numSubst = constructors [ M.Just [ uTerm termDepth
                                                 , uRule termDepth
                                                 , uPosition termDepth
                                                 , uSubstitution numSubst termDepth
                                                 , uTerm termDepth
                                                 ]
                                        ]

uPos = constructors [ M.Just [], M.Just [], M.Just [] ]
uPosition l               = uList l uPos
uSubstitution l termDepth = uList l (uPair uName (uTerm termDepth))

uDerivation numSteps termDepth numSubst = uList numSteps (uStep termDepth numSubst)

uLoopingDerivation numSteps numSubst termDepth = 
  constructors [ M.Just [ uDerivation numSteps termDepth numSubst
                        , uPosition termDepth
                        , uSubstitution numSubst termDepth
                        ]
               ]

result = solveAndTest (uLoopingDerivation 3 2 2) encConstraint constraint

main = result
