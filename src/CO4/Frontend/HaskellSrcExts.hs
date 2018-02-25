{-# LANGUAGE LambdaCase #-}
module CO4.Frontend.HaskellSrcExts
  (toTHDeclarations, toTHDeclaration)
where

import           Data.Generics (everywhere,mkT)
import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.Meta as HM
import qualified Language.Haskell.TH as TH
import           Debug.Trace (trace)
import           CO4.Config (Configs, Config (Profile,ImportPrelude))

toTHDeclarations :: Configs -> HE.Module HE.SrcSpanInfo -> [TH.Dec]
toTHDeclarations configs = \case 
  HE.Module _ _ _ imports decs ->
    if null imports 
    then result
    else trace "Frontend.HaskellSrcExts (Warning): Import declarations will be deleted" result
    where 
      result            = map convert decs
      convert           = toTHDeclaration 
                        . everywhere (mkT $ mapExp markDiscriminants)

      markDiscriminants = (Profile `elem` configs) && (ImportPrelude `elem` configs)

toTHDeclaration :: HE.Decl HE.SrcSpanInfo -> TH.Dec
toTHDeclaration = HM.toDec

mapExp :: Bool -> HE.Exp HE.SrcSpanInfo -> HE.Exp HE.SrcSpanInfo 
mapExp markDiscriminants exp = case exp of
  HE.App loc (HE.Var _ (HE.UnQual _ (HE.Ident _ "assertKnown"))) e ->
    call "assertKnownLoc" loc e

  HE.Case loc discriminant matches | markDiscriminants -> 
    HE.Case loc (call "markedDiscriminant" loc discriminant) matches

  HE.If loc c t f -> 
      mapExp markDiscriminants
    $ HE.Case loc c 
    [ HE.Alt noLoc (HE.PApp noLoc (HE.UnQual noLoc $ HE.Ident noLoc "True" ) []) (HE.UnGuardedRhs noLoc t) Nothing
    , HE.Alt noLoc (HE.PApp noLoc (HE.UnQual noLoc $ HE.Ident noLoc "False") []) (HE.UnGuardedRhs noLoc f) Nothing
    ]

  _ -> exp

  where
    noLoc            = HE.noInfoSpan $ HE.mkSrcSpan HE.noLoc HE.noLoc
    call f loc inner = 
      HE.App noLoc
        (HE.App noLoc 
          (HE.App noLoc
            (HE.Var noLoc (HE.UnQual noLoc (HE.Ident noLoc f)))
            (HE.Lit noLoc $ HE.Int noLoc line $ show line)
          )
          (HE.Lit noLoc $ HE.Int noLoc col $ show col)
        ) inner
      where
        line  = fromIntegral $ HE.startLine   loc
        col   = fromIntegral $ HE.startColumn loc
