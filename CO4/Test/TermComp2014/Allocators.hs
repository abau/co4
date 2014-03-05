module CO4.Test.TermComp2014.Allocators
where

import qualified Data.Map as M
import           CO4.AllocatorData (Allocator,known,constructors)
import           CO4.Prelude (kNil,kList',uBool,kBool,kTuple2)
import           CO4.Util (binaries)
import           CO4.Test.TermComp2014.Standalone (Symbol,Domain,Trs(..),Rule(..),Term(..))
import           CO4.Test.TermComp2014.Util (nodeArities)

modelAllocator :: Int -> Trs () -> Allocator
modelAllocator n = kList' . map goArity . M.toList . nodeArities
  where
    goArity (v,arity)      = kTuple2 (knownSymbolAllocator v) (goInterpretation arity)
    goInterpretation arity = kList' $ do args <- sequence $ replicate arity $ binaries n
                                         return $ goMapping args
    
    goMapping args = kTuple2 (kList' $ map knownValueAllocator args)
                             (unknownValueAllocator n)

precedenceAllocator :: Int -> Trs () -> Allocator
precedenceAllocator n trs = kList' $ concatMap goArity $ M.toList arities
  where
    arities                = nodeArities trs
    labels                 = binaries n
    maxNat                 = (M.size arities) * (length labels)
    goArity (symbol,arity) = do
      args <- sequence $ replicate arity labels
      return $ kTuple2 (kTuple2 (knownSymbolAllocator symbol) 
                                (kList' $ map knownValueAllocator args)
                       ) 
                       (unknownNatAllocator maxNat)

labeledTrsAllocator :: Int -> Trs () -> Allocator
labeledTrsAllocator n (Trs rules) = known 0 1 [ kList' $ concatMap goRule rules ]
  where
    factor                  = length $ binaries n
    goRule (Rule lhs rhs)   = replicate factor $ known 0 1 [ goTerm lhs, goTerm rhs ]
    goTerm (Var v)          = known 0 2 [ knownSymbolAllocator v ]
    goTerm (Node s () args) = known 1 2 [ knownSymbolAllocator s
                                        , unknownLabelAllocator args
                                        , kList' $ map goTerm args
                                        ]

    unknownLabelAllocator = kList' . map (const $ unknownValueAllocator n)

unknownNatAllocator :: Int -> Allocator
unknownNatAllocator 0 = constructors [ Just [], Nothing ]
unknownNatAllocator i = constructors [ Just [], Just [unknownNatAllocator $ i - 1] ]

knownValueAllocator :: Domain -> Allocator
knownValueAllocator = kList' . map kBool
      
unknownValueAllocator :: Int -> Allocator
unknownValueAllocator n = kList' $ replicate n uBool

knownSymbolAllocator :: Symbol -> Allocator
knownSymbolAllocator = knownValueAllocator

unknownSymbolAllocator :: Int -> Allocator
unknownSymbolAllocator = unknownValueAllocator
