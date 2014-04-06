{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)

import System.Environment (getArgs)
import System.IO

import           Language.Haskell.TH.Syntax (addDependentFile)

$( compileFile [ImportPrelude
               -- ,DumpAll "/tmp/WCB"
               , Cache
               , Profile
               ] 
   "CO4/Test/WCB.standalone.hs" )

-- uBase = constructors [ Just [], Just [], Just [], Just []]
uBase = known 0 1 [ kList 2 uBool ]

inforna cs = map ( \ c -> case c of
    '(' -> Open ; '.' -> Blank ; ')' -> Close ) cs

ex1 = inforna "(((((...(((((......))))).)))))"

ex0 = [Open,Open
     ,Blank,Close,Open
     ,Close ,Close,Blank 
    ]

-- allocator = kList size uBase

result_for sec = do
    out <- solveAndTestP 
       sec 
       (kList (length sec) uBase) 
       encConstraint constraint
    case out of
        Nothing -> putStrLn "Nothing"
        Just prim -> do
            putStrLn $ map unParen sec
            putStrLn $ map unBase prim

unParen s = case s of Open -> '(' ; Blank -> '.' ; Close -> ')'
unBase b = applyB b (basetree 'A' 'C' 'G' 'U')

main = do
    hSetBuffering stdout LineBuffering
    [ arg1 ] <- getArgs
    result_for $ inforna arg1
