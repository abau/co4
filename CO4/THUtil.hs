{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module CO4.THUtil 
where

import           Data.Generics (GenericT,everywhere,mkT)
import qualified Language.Haskell.TH as TH
import qualified Data.Map as M
import           CO4.Names

funD :: Namelike n => n -> [TH.Clause] -> TH.Dec
funD n = TH.FunD $ toTHName n

funD' :: Namelike n => n -> [TH.Pat] -> TH.Exp -> TH.Dec
funD' n pats exp = TH.FunD (toTHName n) [TH.Clause pats (TH.NormalB exp) []]

valD' :: Namelike n => n -> TH.Exp -> TH.Dec
valD' n exp = TH.ValD (varP n) (TH.NormalB exp) []

varE :: Namelike n => n -> TH.Exp
varE = TH.VarE . toTHName

conE :: Namelike n => n -> TH.Exp
conE = TH.ConE . toTHName

appsE :: TH.Exp -> [TH.Exp] -> TH.Exp
appsE = foldl TH.AppE 

lamE' :: Namelike n => [n] -> TH.Exp -> TH.Exp
lamE' ns = TH.LamE $ map varP ns

letE' :: Namelike n => [(n,TH.Exp)] -> TH.Exp -> TH.Exp
letE' bindings = TH.LetE $ map (uncurry valD') bindings

intE :: Int -> TH.Exp
intE = TH.LitE . TH.IntegerL . fromIntegral

stringE :: Namelike n => n -> TH.Exp
stringE = TH.LitE . TH.StringL . fromName

returnE :: TH.Exp -> TH.Exp
returnE = TH.AppE $ TH.VarE 'return

bindS' :: Namelike n => n -> TH.Exp -> TH.Stmt
bindS' n = TH.BindS $ varP n

varT :: Namelike n => n -> TH.Type
varT = TH.VarT . toTHName

conT :: Namelike n => n -> TH.Type
conT = TH.ConT . toTHName

appsT :: TH.Type -> [TH.Type] -> TH.Type
appsT = foldl TH.AppT 

varP :: Namelike n => n -> TH.Pat
varP = TH.VarP . toTHName

conP :: Namelike n => n -> [TH.Pat] -> TH.Pat
conP n = TH.ConP $ toTHName n

intP :: Int -> TH.Pat
intP = TH.LitP . TH.IntegerL . fromIntegral

normalC' :: Namelike n => n -> [TH.Type] -> TH.Con
normalC' n = TH.NormalC (toTHName n) . map (\t -> (TH.NotStrict,t))

toTHName :: Namelike n => n -> TH.Name
toTHName = TH.mkName . fromName

deleteSignatures :: GenericT
deleteSignatures = everywhere (mkT noSigE)
                 . everywhere (mkT noSigP)
                 . everywhere (mkT noSigD)
  where
    noSigE exp = case exp of TH.SigE e _ -> e
                             _           -> exp

    noSigP pat = case pat of TH.SigP p _ -> p
                             _           -> pat

    noSigD = filter (not . isSig)
    
    isSig (TH.SigD {}) = True
    isSig _            = False

deleteTypeSynonyms :: GenericT
deleteTypeSynonyms = everywhere (mkT noTypeSyn)
  where
    noTypeSyn                = filter (not . isTypeSyn)
    isTypeSyn (TH.TySynD {}) = True
    isTypeSyn _              = False

renameTHNames :: [(TH.Name,TH.Name)] -> GenericT
renameTHNames renamings = 
  let renamings'  = M.fromList renamings
      rename name = M.findWithDefault name name renamings'
  in
    everywhere (mkT rename)

typedUndefined :: TH.Type -> TH.Exp
typedUndefined = TH.SigE $ TH.VarE 'undefined

typedWildcard :: TH.Type -> TH.Pat
typedWildcard = TH.SigP TH.WildP

unqualifiedNames :: GenericT
unqualifiedNames = everywhere $ mkT unqualifiedName
  where
    unqualifiedName = TH.mkName . TH.nameBase

deriveShows :: GenericT
deriveShows = everywhere $ mkT deriveShow
  where
    deriveShow (TH.DataD ctxt name tvars cons names)
      | not (''Show `elem` names) = TH.DataD ctxt name tvars cons $ names ++ [''Show]

    deriveShow decl = decl
