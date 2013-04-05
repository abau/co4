{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CO4.Test.Closure
where

import qualified Prelude ; import Prelude (($), (-))

import           Data.Maybe
import qualified GHC.Types

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

uRule wordLength = constructors [ Just [ uList wordLength uSigma
                                       , uList wordLength uSigma ] ]

kNil       = known 0 2 []
kCons x xs = known 1 2 [ x , xs ]
kList 0 _  = kNil
kList i a  = kCons a (kList (i-1) a)

kRule wordLength = known 0 1 [ kList wordLength uSigma
                             , kList wordLength uSigma
                             ]

uStep  rw w = known 0 1 [ uList w uSigma
                        , kRule rw
                        , uList w uSigma
                        ]

kStep  rw w = known 0 1 [ kList w uSigma
                        , kRule rw
                        , kList w uSigma
                        ]

allocator rw w l = ( uList l (uStep rw w))

allokator rw w l = ( kList l (kStep rw w))

result = solveAndTestBoolean GHC.Types.True (allocator 4 20 20)  encMain main
