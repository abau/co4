{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.TermComp2014.Main
where

import           Prelude hiding (lookup)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)
import           CO4.Test.TermComp2014.Trs
import           CO4.Test.TermComp2014.PPrint
import           Unsafe.Coerce

$( compileFile [Dump "/tmp/termcomp",ImportPrelude] "CO4/Test/TermComp2014/Standalone.hs" )

result :: FilePath -> IO ()
result filePath = do
  trs      <- parseTrs filePath
  solution <- solveAndTestP (parameter trs) 
                            (allocator trs) 
                            encConstraint constraint
  case solution of
    Nothing    ->    putStrLn "Nothing"
    Just model -> do putStrLn $ pprintLabeledTrs bitWidth trs $ unsafeCoerce model
                     putStrLn $ pprintModel                   $ unsafeCoerce model

bitWidth      = 2
parameter trs = (unsafeCoerce trs, unsafeCoerce $ assignments bitWidth trs)
allocator trs = modelAllocator bitWidth trs
