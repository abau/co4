{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.Transport where

import qualified Prelude ; import Prelude (($), (-), (*))

import           Data.Maybe

import           Language.Haskell.TH (runIO)

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4

$( compileFile [Verbose] "CO4/Test/Transport.standalone.hs" )

uBool      = constructors [ Just [] , Just [] ]
uSigma     = constructors [ Just [] , Just [], Just [] ]
uList 0 _  = constructors [ Just [] , Nothing ]
uList i a  = constructors [ Just [] , Just [a, uList (i-1) a ] ]

uWord k = uList k uSigma
uBlock k = uList k (uWord k)

-- data Rule = Rule (List Sigma) (List Sigma)
uRule wordLength = constructors [ Just [ uList wordLength uSigma
                                       , uList wordLength uSigma ] ]


uStep  rw w = known 0 1 [ uWord w , uRule rw , uWord w ]


-- type Derivation = List Step

-- data Move = Move (List Sigma) -- ^ origin (block letter)
--                 (List (List Sigma)) -- ^ image (concatenation of block letters)
--                 (List Step)  -- ^ origin . pivot ->> pivot . image

uMove rw k = known 0 1 [ uWord k , uBlock k, uList k (uStep rw k) ]

-- type Morphism = (List Move)

uMorph rw k = uList k (uMove rw k)

-- data Image = Image (List (List Sigma)) -- ^  phi^k (start)
--                   ( List (List Sigma)) -- ^  start ^ pivot^k

uImage l k = known 0 1 [ uList l (uWord k), uList l (uWord k) ]


-- data Transport = Transport (List Sigma) -- ^ pivot
--                            (List Move)  -- ^ morphism
--                            (List Sigma) -- ^ start
--                           (List Image)

uTransport rw l k = known 0 1 [ uWord k, uMorph rw k, uWord k, uList 6 (uImage l k) ]

allocator = uTransport 5 19 3

result = solveAndTest allocator encMain main

