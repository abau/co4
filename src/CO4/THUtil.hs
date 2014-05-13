{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module CO4.THUtil 
where

import           Data.Generics (GenericT,everywhere,mkT)
import qualified Language.Haskell.TH as TH
import qualified Data.Map as M
import           CO4.Names
import           CO4.Util (extT')
import           CO4.Frontend.THPreprocess 
  (noSignatureExpression,noSignaturePattern,noSignatureDeclarations)

funD :: Namelike n => n -> [TH.Clause] -> TH.Dec
funD n = TH.FunD $ toTHName n

funD' :: Namelike n => n -> [TH.Pat] -> TH.Exp -> TH.Dec
funD' n pats exp = TH.FunD (toTHName n) [TH.Clause pats (TH.NormalB exp) []]

valD' :: Namelike n => n -> TH.Exp -> TH.Dec
valD' n exp = TH.ValD (varP n) (TH.NormalB exp) []

sigD' :: Namelike n => n -> TH.Type -> TH.Dec
sigD' n = TH.SigD (toTHName n)

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

caseE :: TH.Exp -> [(TH.Pat,TH.Exp)] -> TH.Exp
caseE d = TH.CaseE d . map (\(p,m) -> TH.Match p (TH.NormalB m) [])

intE :: Integral a => a -> TH.Exp
intE = TH.LitE . TH.IntegerL . toInteger

stringE :: Namelike n => n -> TH.Exp
stringE = TH.LitE . TH.StringL . fromName

returnE :: TH.Exp -> TH.Exp
returnE = TH.AppE $ TH.VarE 'return

bindS' :: Namelike n => n -> TH.Exp -> TH.Stmt
bindS' n = TH.BindS $ varP n

varT :: Namelike n => n -> TH.Type
varT = TH.VarT . toTHName

conT :: Namelike n => n -> TH.Type
conT name = if fromName name == listName
            then TH.ListT
            else TH.ConT $ toTHName name

appsT :: TH.Type -> [TH.Type] -> TH.Type
appsT = foldl TH.AppT 

varP :: Namelike n => n -> TH.Pat
varP = TH.VarP . toTHName

conP :: Namelike n => n -> [TH.Pat] -> TH.Pat
conP n = TH.ConP $ toTHName n

intP :: Integral a => a -> TH.Pat
intP = TH.LitP . TH.IntegerL . toInteger

normalC' :: Namelike n => n -> [TH.Type] -> TH.Con
normalC' n = TH.NormalC (toTHName n) . map (\t -> (TH.NotStrict,t))

toTHName :: Namelike n => n -> TH.Name
toTHName = TH.mkName . fromName

deleteSignatures :: GenericT
deleteSignatures = everywhere $ extT' noSignatureDeclarations
                              $ extT' noSignaturePattern
                              $ mkT   noSignatureExpression

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

derive :: TH.Name -> GenericT
derive n = everywhere $ mkT go
  where
    go (TH.DataD ctxt name tvars cons names)
      | not (n `elem` names) = TH.DataD ctxt name tvars cons $ names ++ [n]

    go decl = decl
