{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.LPOSL
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Util (bitWidth)
import           CO4.Prelude
import           CO4.Example.LPOSLStandalone

$( compileFile [Cache,ImportPrelude] "test/CO4/Example/LPOSLStandalone.hs" )

parameter :: (Trs, [Sigma])
allocator :: TAllocator (Precedence, Interpretation)
(parameter, allocator) = 
  let funSym@  [tSym,pSym,fSym,gSym,aSym] = map nat [0 .. 4]
      arities = [ 2 ,  2 ,  1 ,  2 ,  0 ]

      varSym@[xSym,ySym,zSym] = map nat [0 .. 2]

      t  a b = Node tSym  [a,b]
      p  a b = Node pSym  [a,b]
      f  a   = Node fSym  [a]
      g  a b = Node gSym  [a,b]
      a      = Node aSym  []

      x      = Var xSym
      y      = Var ySym
      z      = Var zSym

      trs    = [ (t (t x y) z    , t x (t y z))
               , (t (p  x y) z   , p (t x z) (t y z))
               , (t x (p y (f z)), t (g x z) (p y a)) ]

      carrierSize  = 2
      carrier      = map nat [0 .. (carrierSize - 1)]

      argumentAllocs arity = map allocatorList
                           $ sequence 
                           $ replicate arity
                           $ map fromKnown carrier

      assignments = map (zip varSym)
                  $ sequence 
                  $ replicate (length varSym) carrier

      precWidth  = 3
      precAlloc  = allocatorList 
                 $ concatMap (\(sym,arity) -> 
                       map (\a -> knownTuple2 (knownTuple2 (fromKnown sym) a) (uNat precWidth)) 
                     $ argumentAllocs arity
                   )
                 $ zip funSym arities

      interAlloc = allocatorList
                 $ map (\(sym,arity) -> 
                       knownTuple2 (fromKnown sym)
                     $ allocatorList
                     $ map (\a -> knownTuple2 a (uNat $ fromInteger carrierSize)) 
                     $ argumentAllocs arity
                   )
                 $ zip funSym arities
  in
    ((trs, assignments), knownTuple2 precAlloc interAlloc)

result = solveAndTestP parameter allocator encConstraint constraint
{-
result = do
  r <- solveAndTestP parameter allocator encConstraint constraint
  case r of
    Nothing -> return ()
    Just (p,i) -> do
      let ltrs = labelledTrs i (snd parameter) (fst parameter)

      putStrLn $ showTrs showTerm $ fst parameter
      putStrLn $ showInterpretation i
      putStrLn $ showTrs showLTerm ltrs
      putStrLn $ showPrec p

showTrs showT = unlines . map (\(lhs,rhs) -> unwords [ showT lhs, "->", showT rhs ])

showTerm t = case t of Var v -> showVarSym v
                       Node f ts -> unwords [showFunSym f, "(", unwords $ map showTerm ts, ")"]

showLTerm t = case t of LVar v -> showVarSym v
                        LNode f l ts -> unwords [showFunSym f, showArgs l
                                                , "(", unwords $ map showLTerm ts, ")"]

showVarSym v = case value v of 0 -> "x"
                               1 -> "y"
                               2 -> "z"

showFunSym f = case value f of 0 -> "t"
                               1 -> "p"
                               2 -> "f"
                               3 -> "g"
                               4 -> "a"

showInterpretation = unlines . map (\(s,f) -> showFunction s f)

showArgs args = unwords ["[", unwords $ map (show . value) args, "]"]

showFunction s = unlines . map (\(args,r) -> unwords [ showFunSym s, showArgs args, show $ value r ])

showPrec = unlines . map (\((s,l),p) -> unwords [showFunSym s, showArgs l, ": ", show $ value p])
-}
