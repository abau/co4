{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CO4.Test.Closure
where

import qualified Prelude ; import Prelude (($), (-))

import           Data.Maybe

import           Language.Haskell.TH (runIO)
import qualified Language.Haskell.Exts as HE

import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4


$( runIO $ do 
   full_input <- Prelude.readFile "CO4/Test/Closure.standalone.hs" 
   let -- die ersten Zeilen werden für stand-alone benötigt, stören aber hier:
       -- "module .. where" benötigt, weil sonst main den falschen Typ hat
       -- "import qualified Prelude ; undefined = Prelude.undefined"
       input = Prelude.unlines $ Prelude.drop 2 $ Prelude.lines full_input 
   case HE.parseModule input of
       HE.ParseOk p -> configurable [Verbose] $ compile p 
 )

uBool      = constructors [ Just [] , Just [] ]
uSigma     = constructors [ Just [] , Just [] ]
uList 0 _  = constructors [ Just [] , Nothing ]
uList i a  = constructors [ Just [] , Just [a, uList (i-1) a ] ]

-- data Rule = Rule (List Sigma) (List Sigma)
uRule wordLength = constructors [ Just [ uList wordLength uSigma
                                       , uList wordLength uSigma ] ]
uClosure = uRule

kNil       = known 0 2 []
kCons x xs = known 1 2 [ x , xs ]
kList 0 _  = kNil
kList i a  = kCons a (kList (i-1) a)


-- data Side = Left | Right | Inside | Outside

uSide = constructors $ Prelude.replicate 4 $ Just [] 

-- data Overlap = Overlap Side (List Sigma) (List Sigma) Rule Rule

uOverlap w = known 0 1 [ uSide, uList w uSigma, uList w uSigma, uClosure w, uClosure w ]

-- data Step = Step Rule Overlap 

uStep w = known 0 1 [ uClosure w, uOverlap w ]

-- type Derivation = List Step

allocator l w = ( uList l (uStep w ))

result = solveAndTest (allocator 10 15 )  encMain main
