{-# LANGUAGE LambdaCase #-}
module CO4.Test.TermComp2014.Main
where

import           Unsafe.Coerce
import           CO4.Test.TermComp2014.Util (parseTrs,toLabeledTrs)
import           CO4.Test.TermComp2014.PPrint (pprintModel,pprintLabeledTrs,pprintPrecedence')
import qualified CO4.Test.TermComp2014.SL.Main as SL
import qualified CO4.Test.TermComp2014.LPO.Main as LPO

resultFile :: Int -> FilePath -> IO ()
resultFile bitWidth filePath = do
  trs <- parseTrs filePath
  SL.result bitWidth trs >>= \case 
    Nothing    -> putStrLn "SL: Nothing"
    Just model ->
      let (labeledTrs, mapping) = toLabeledTrs bitWidth trs model
      in do
        putStrLn $ pprintLabeledTrs bitWidth trs $ unsafeCoerce model
        putStrLn $ pprintModel                   $ unsafeCoerce model
        LPO.result labeledTrs >>= \case
          Nothing   -> putStrLn "LPO: Nothing"
          Just prec -> putStrLn $ pprintPrecedence' mapping $ unsafeCoerce prec
