{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CO4.Example.Binary
where

import           Language.Haskell.TH (runIO)
import qualified Satchmo.Core.SAT.Minisat
import qualified Satchmo.Core.Decode 
import           CO4
import           CO4.Prelude
import           CO4.Util (toBinary,fromBinary)

$( runIO $ configurable [ImportPrelude,Cache] 
         $ compileFile "CO4/Example/Binary.standalone.hs" )

bitWidth  = 8
uNat      = uList bitWidth uBool
allocator = uTuple2 uNat uNat

result :: Int -> IO (Maybe (Int,Int))
result x = do
  solution <- solveAndTestBooleanP (toBinary (Just bitWidth) x) booleanCache
                                   allocator encConstraint constraint
  case solution of
    Nothing    -> return Nothing
    Just (a,b) -> return $ Just (fromBinary a, fromBinary b)
