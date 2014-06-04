{-# LANGUAGE LambdaCase #-}
module CO4.Frontend.HaskellSrcExts
  (toTHDeclarations, toTHDeclaration)
where

import           Data.Generics (everywhere,mkT)
import qualified Language.Haskell.Exts.Annotated as HEA
import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.Exts.SrcLoc as HE
import           Language.Haskell.Exts.Annotated.Simplify (sDecl)
import qualified Language.Haskell.Meta as HM
import qualified Language.Haskell.TH as TH
import           Debug.Trace (trace)

toTHDeclarations :: HEA.Module HEA.SrcSpanInfo -> [TH.Dec]
toTHDeclarations = \case 
  HEA.Module _ _ _ imports decs ->
    if null imports 
    then result
    else trace "Frontend.HaskellSrcExts (Warning): Import declarations will be deleted" result
    where 
      result  = map convert decs
      convert = toTHDeclaration . sDecl . everywhere (mkT toAssertKnownLoc)

toTHDeclaration :: HE.Decl -> TH.Dec
toTHDeclaration = HM.toDec

toAssertKnownLoc :: HEA.Exp HEA.SrcSpanInfo -> HEA.Exp HEA.SrcSpanInfo 
toAssertKnownLoc exp = case exp of
  HEA.App loc (HEA.Var _ (HEA.UnQual _ (HEA.Ident _ "assertKnown"))) e ->
    HEA.App noLoc
      (HEA.App noLoc 
        (HEA.App noLoc
          (HEA.Var noLoc (HEA.UnQual noLoc (HEA.Ident noLoc "assertKnownLoc")))
          (HEA.Lit noLoc $ HEA.Int noLoc line $ show line)
        )
        (HEA.Lit noLoc $ HEA.Int noLoc col  $ show col)
      ) e
    where
      line  = fromIntegral $ HE.startLine   loc
      col   = fromIntegral $ HE.startColumn loc
      noLoc = HE.noInfoSpan $ HE.mkSrcSpan HE.noLoc HE.noLoc
  _ -> exp
