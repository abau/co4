{-# LANGUAGE LambdaCase #-}
module CO4.Frontend.HaskellSrcExts
  (toTHDeclarations)
where

import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.Meta as HM
import qualified Language.Haskell.TH as TH
import           Debug.Trace (trace)

toTHDeclarations :: HE.Module -> [TH.Dec]
toTHDeclarations = \case 
  HE.Module _ _ [] Nothing _ imports decs ->
    if null imports 
    then p
    else trace "Frontend.HaskellSrcExts (Warning): Import declarations will be deleted" p
    where 
      p = HM.toDecs decs

  _ -> error $ "Frontend.HaskellSrcExts.parseProgram: no pragmas or warning texts allowed"
