{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import           Prelude hiding (lex,lookup,length)
import           System.Environment (getArgs)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Test.TermComp2014.Util (parseTrs,assignments)
import           CO4.Test.TermComp2014.PPrint
import           CO4.Test.TermComp2014.Allocators
import           Unsafe.Coerce

$( compileFile [Cache,ImportPrelude] "CO4/Test/TermComp2014/Standalone.hs" )

resultFile :: Int -> FilePath -> IO ()
resultFile bitWidth filePath = do
  trs <- parseTrs filePath
  
  let sigmas    = assignments bitWidth trs
      parameter = (unsafeCoerce trs, unsafeCoerce sigmas)
      allocator = kTuple2 (modelAllocator      bitWidth trs)
                          (precedenceAllocator bitWidth trs)

  solveAndTestP parameter allocator encConstraint constraint
    >>= return . fmap unsafeCoerce
    >>= \case
          Nothing -> putStrLn "Nothing"
          Just (model,precedence) -> 
            do putStrLn $ pprintModel               $ unsafeCoerce model
               putStrLn $ pprintLabeledTrs bitWidth $ unsafeCoerce labeledTrs
               putStrLn $ pprintPrecedence          $ unsafeCoerce precedence
            where
              labeledTrs = makeLabeledTrs model (unsafeCoerce trs) sigmas

main :: IO ()
main = do
  [n, filePath] <- getArgs
  resultFile (read n) filePath
