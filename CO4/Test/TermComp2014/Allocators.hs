module CO4.Test.TermComp2014.Allocators
where

import           Control.Exception (assert)
import qualified Data.Map as M
import           CO4.AllocatorData (Allocator,known,constructors)
import           CO4.Prelude (kNil,kList',uBool,kBool,uTuple2)
import           CO4.Util (toBinary)
import           CO4.Test.TermComp2014.Data
import           CO4.Test.TermComp2014.Util

modelAllocator :: Int -> Trs -> Allocator
modelAllocator n (Trs rules) = kList' $ map goArity $ M.toList arities
  where
    arities = M.fromListWith (\a b -> assert (a == b) a) $ concatMap goRule rules
      where goRule (Rule l r)    = (goTerm l) ++ (goTerm r)
            goTerm (Var {})      = []
            goTerm (Node v args) = (v, length args) : (concatMap goTerm args)

    goArity (v,arity)      = uTuple2 (knownSymbolAllocator v) (goInterpretation arity)
    goInterpretation arity = kList' $ do args <- sequence $ replicate arity [0..numStates - 1]
                                         return $ goMapping args
      where
        numStates = 2^n
    
    goMapping args = uTuple2 (kList' $ map (knownValueAllocator n) args)
                             (unknownValueAllocator n)

precedenceAllocator :: Trs -> Allocator
precedenceAllocator trs = kList' $ map goMapping symbols
  where
    symbols             = nodeSymbols trs
    goMapping symbol    = uTuple2 (knownSymbolAllocator symbol) $ unknownNatAllocator 
                                                                $ length symbols
    unknownNatAllocator 0 = constructors [ Just [], Nothing ]
    unknownNatAllocator i = constructors [ Just [], Just [unknownNatAllocator $ i - 1] ]

knownValueAllocator :: Int -> Int -> Allocator
knownValueAllocator n l = kList' $ map kBool $ toBinary (Just n) l
      
unknownValueAllocator :: Int -> Allocator
unknownValueAllocator n = kList' $ replicate n uBool

knownSymbolAllocator :: Symbol -> Allocator
knownSymbolAllocator = kList' . map kBool

unknownSymbolAllocator :: Int -> Allocator
unknownSymbolAllocator = unknownValueAllocator
