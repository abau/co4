{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Test.TermComp2014.LPO.Main
where

import           Prelude hiding (lex,lookup)
import           Unsafe.Coerce
import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude hiding (Nat)
import           CO4.Test.TermComp2014.Util (parseTrs)
import           CO4.Test.TermComp2014.Allocators (precedenceAllocator)
import           CO4.Test.TermComp2014.PPrint (pprintPrecedence)
import qualified CO4.Test.TermComp2014.Data as Data

$( compileFile [ImportPrelude] "CO4/Test/TermComp2014/LPO/Standalone.hs" )

resultFile :: FilePath -> IO ()
resultFile filePath = do
  solution <- parseTrs filePath >>= result
  case solution of
    Nothing   ->    putStrLn "Nothing"
    Just prec -> do putStrLn $ pprintPrecedence $ unsafeCoerce prec

result :: Data.Trs -> IO (Maybe Data.Precedence)
result trs = solveAndTestP parameter allocator encConstraint constraint
         >>= return . fmap unsafeCoerce
  where
    parameter = unsafeCoerce trs
    allocator = precedenceAllocator trs
