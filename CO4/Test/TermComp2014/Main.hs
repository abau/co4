{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module CO4.Test.TermComp2014.Main
where

import           Prelude hiding (lex,lookup,length)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude hiding (Nat,eqNat,encEqNat)
import           CO4.Test.TermComp2014.Util (parseTrs,assignments)
import           CO4.Test.TermComp2014.PPrint
import           CO4.Test.TermComp2014.Allocators
import           Unsafe.Coerce

$( compileFile [Dump "/tmp/termcomp2014", ImportPrelude] "CO4/Test/TermComp2014/Standalone.hs" )

resultFile :: Int -> FilePath -> IO ()
resultFile bitWidth filePath = do
  trs      <- parseTrs filePath
  
  let parameter = (unsafeCoerce trs, unsafeCoerce $ assignments bitWidth trs)
      allocator = kTuple3 (modelAllocator      bitWidth trs)
                          (labeledTrsAllocator bitWidth trs)
                          (precedenceAllocator bitWidth trs)

  solveAndTestP parameter allocator encConstraint constraint
    >>= return . fmap unsafeCoerce
    >>= \case
          Nothing -> putStrLn "Nothing"
          Just (model,labeledTrs,precedence) -> 
            do putStrLn $ pprintModel               $ unsafeCoerce model
               putStrLn $ pprintLabeledTrs bitWidth $ unsafeCoerce labeledTrs
               putStrLn $ pprintPrecedence          $ unsafeCoerce precedence
