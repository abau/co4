{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.TermComp2014.SL.Main
where

import           Prelude hiding (lookup)
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)
import           CO4.Test.TermComp2014.Util (parseTrs,assignments)
import           CO4.Test.TermComp2014.PPrint (pprintModel, pprintLabeledTrs)
import           CO4.Test.TermComp2014.Allocators (modelAllocator)
import qualified CO4.Test.TermComp2014.Data as Data
import           Unsafe.Coerce

$( compileFile [ImportPrelude] "CO4/Test/TermComp2014/SL/Standalone.hs" )

resultFile :: Int -> FilePath -> IO ()
resultFile bitWidth filePath = do
  trs      <- parseTrs filePath
  solution <- result bitWidth trs
  case solution of
    Nothing    ->    putStrLn "Nothing"
    Just model -> do putStrLn $ pprintLabeledTrs bitWidth trs $ unsafeCoerce model
                     putStrLn $ pprintModel                   $ unsafeCoerce model

result :: Int -> Data.Trs -> IO (Maybe Data.Model)
result bitWidth trs = solveAndTestP parameter allocator encConstraint constraint
                  >>= return . fmap unsafeCoerce
  where
    parameter = (unsafeCoerce trs, unsafeCoerce $ assignments bitWidth trs)
    allocator = modelAllocator bitWidth trs
