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
import           CO4.Config (Configs, Config (Profile,ImportPrelude))

toTHDeclarations :: Configs -> HEA.Module HEA.SrcSpanInfo -> [TH.Dec]
toTHDeclarations configs = \case 
  HEA.Module _ _ _ imports decs ->
    if null imports 
    then result
    else trace "Frontend.HaskellSrcExts (Warning): Import declarations will be deleted" result
    where 
      result            = map convert decs
      convert           = toTHDeclaration 
                        . sDecl 
                        . everywhere (mkT $ mapExp markDiscriminants)

      markDiscriminants = (Profile `elem` configs) && (ImportPrelude `elem` configs)

toTHDeclaration :: HE.Decl -> TH.Dec
toTHDeclaration = HM.toDec

mapExp :: Bool -> HEA.Exp HEA.SrcSpanInfo -> HEA.Exp HEA.SrcSpanInfo 
mapExp markDiscriminants exp = case exp of
  HEA.App loc (HEA.Var _ (HEA.UnQual _ (HEA.Ident _ "assertKnown"))) e ->
    call "assertKnownLoc" loc e

  HEA.Case loc discriminant matches | markDiscriminants -> 
    HEA.Case loc (call "markedDiscriminant" loc discriminant) matches

  HEA.If loc c t f -> 
      mapExp markDiscriminants
    $ HEA.Case loc c 
    [ HEA.Alt noLoc (HEA.PApp noLoc (HEA.UnQual noLoc $ HEA.Ident noLoc "True" ) []) (HEA.UnGuardedAlt noLoc t) Nothing
    , HEA.Alt noLoc (HEA.PApp noLoc (HEA.UnQual noLoc $ HEA.Ident noLoc "False") []) (HEA.UnGuardedAlt noLoc f) Nothing
    ]

  _ -> exp

  where
    noLoc            = HE.noInfoSpan $ HE.mkSrcSpan HE.noLoc HE.noLoc
    call f loc inner = 
      HEA.App noLoc
        (HEA.App noLoc 
          (HEA.App noLoc
            (HEA.Var noLoc (HEA.UnQual noLoc (HEA.Ident noLoc f)))
            (HEA.Lit noLoc $ HEA.Int noLoc line $ show line)
          )
          (HEA.Lit noLoc $ HEA.Int noLoc col $ show col)
        ) inner
      where
        line  = fromIntegral $ HE.startLine   loc
        col   = fromIntegral $ HE.startColumn loc
