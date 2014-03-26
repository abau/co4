module CO4.Test.TermComp2014.Allocators
where

import           Control.Exception (assert)
import qualified Data.Map as M
import           CO4.AllocatorData (Allocator)
import           CO4.Prelude (kList,kList',uBool,kBool,kTuple2,uNat)
import           CO4.Util (binaries,bitWidth)
import           CO4.Test.TermComp2014.Standalone (Symbol,Domain,Trs(..),DPTrs(..),MarkedSymbol,Label)
import           CO4.Test.TermComp2014.Util (nodeArities,dpToTrs)

allocator :: Int -> Int -> DPTrs () -> Allocator
allocator bitWidth numPrecedences dpTrs = 
  kTuple2 (modelAllocator kMarkedSymbolAllocator bitWidth trs)
          (kList numPrecedences $ precedenceAllocator kMarkedSymbolAllocator bitWidth trs)
  where
    trs = dpToTrs dpTrs

modelAllocator :: Ord s => (s -> Allocator) -> Int -> Trs v s () -> Allocator
modelAllocator allocSymbol n = kList' . map goArity . M.toList . nodeArities
  where
    goArity (v,arity)      = kTuple2 (allocSymbol v) (goInterpretation arity)
    goInterpretation arity = kList' $ do args <- sequence $ replicate arity $ binaries n
                                         return $ goMapping args
    
    goMapping args = kTuple2 (kList' $ map kValueAllocator args)
                             (uValueAllocator n)

precedenceAllocator :: Ord s => (s -> Allocator) -> Int -> Trs v s () -> Allocator
precedenceAllocator allocSymbol n trs = kList' $ concatMap goArity $ M.toList arities
  where
    arities                = nodeArities trs
    labels                 = binaries n
    numLabeledSymbols      = (M.size arities) * (length labels)
    goArity (symbol,arity) = do
      args <- sequence $ replicate arity labels
      return $ kTuple2 (kTuple2 (allocSymbol symbol) 
                                (kLabelAllocator args)
                       ) 
                       (uNat $ bitWidth numLabeledSymbols)

kValueAllocator :: Domain -> Allocator
kValueAllocator = kList' . map kBool
  
uValueAllocator :: Int -> Allocator
uValueAllocator n = kList' $ replicate n uBool

kSymbolAllocator :: Symbol -> Allocator
kSymbolAllocator s = assert (not $ null s) $ kValueAllocator s

kMarkedSymbolAllocator :: MarkedSymbol -> Allocator
kMarkedSymbolAllocator (s,m) = kTuple2 (kSymbolAllocator s) (kBool m)

kLabelAllocator :: Label -> Allocator
kLabelAllocator = kList' . map kValueAllocator

uLabelAllocator :: Int -> Int -> Allocator
uLabelAllocator n numArgs = kList numArgs $ uValueAllocator n
