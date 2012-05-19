{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module CO4.Backend.TH
  (isTupleCon)
where

import qualified Language.Haskell.TH as TH
import           CO4.Backend
import           CO4.Language
import           CO4.Names

instance ProgramBackend [TH.Dec] where
  displayProgram = concatMap displayDeclaration

instance ExpressionBackend TH.Exp where
  displayExpression exp = case exp of
    EVar v                   -> TH.VarE $ toName v
    ECon c | c == tupleCon 2 -> TH.ConE $ TH.tupleDataName 2
    ECon c | c == tupleCon 3 -> TH.ConE $ TH.tupleDataName 3
    ECon c | c == tupleCon 4 -> TH.ConE $ TH.tupleDataName 4
    ECon c | c == tupleCon 5 -> TH.ConE $ TH.tupleDataName 5
    ECon c | c == tupleCon 6 -> TH.ConE $ TH.tupleDataName 6
    ECon c                   -> TH.ConE $ toName c

    ELit l -> TH.LitE $ displayLiteral l

    EApp (ECon c) args | isTupleCon c -> TH.TupE $ map display args

    EApp e args -> foldl TH.AppE (display e) $ map display args

    ELam ns e   -> TH.LamE (map (TH.VarP . toName) ns) $ display e
    ECase e ms  -> TH.CaseE (display e) $ map displayMatch ms
    ELet n v e  -> TH.LetE (displayDeclaration $ DBind n v) $ display e
    where
      display = displayExpression

instance SchemeBackend TH.Type where
  displayScheme scheme = case scheme of
    SType t -> displayType t
    SForall n s -> 
      let tvar = TH.PlainTV $ toName n
      in
        case displayScheme s of
          TH.ForallT bndrs [] t -> TH.ForallT (tvar : bndrs) [] t
          t                     -> TH.ForallT [tvar]         [] t

displayDeclaration :: Declaration -> [TH.Dec]
displayDeclaration (DBind (NTyped n s) e) = 
  (TH.SigD (toName $ name n) $ displayScheme s) : (displayDeclaration $ DBind (name n) e)
displayDeclaration (DBind n e) = 
  [ TH.ValD (TH.VarP $ toName n) (TH.NormalB $ displayExpression e) [] ]

displayType :: Type -> TH.Type
displayType type_ = case type_ of
  TVar v                       -> TH.VarT $ toName v
  TCon c [a,b] | c == funType  -> TH.AppT (TH.AppT TH.ArrowT $ display a) $ display b
  TCon c [a]   | c == listType -> TH.AppT TH.ListT $ display a
  TCon c args  | isTupleType c -> foldl TH.AppT (TH.TupleT (length args)) $ map display args
  TCon c args                  -> foldl TH.AppT (TH.ConT $ toName c) $ map display args
  where
    display = displayType

displayMatch :: Match -> TH.Match
displayMatch (Match p e) = 
  TH.Match (displayPattern p) (TH.NormalB $ displayExpression e) []

displayPattern :: Pattern -> TH.Pat
displayPattern pattern = case pattern of
  PVar v    -> TH.VarP $ toName v
  PLit l    -> TH.LitP $ displayLiteral l
  PCon n ps | isTupleCon n -> TH.TupP $ map displayPattern ps
  PCon n ps                -> TH.ConP (toName n) $ map displayPattern ps

displayLiteral :: Literal -> TH.Lit
displayLiteral literal = case literal of
  LInt l    -> TH.IntegerL l
  LChar l   -> TH.CharL l
  LDouble l -> TH.RationalL $ toRational l

toName :: Namelike n => n -> TH.Name
toName n | fromName n == fromName nilCon   = '[]
toName n | fromName n == fromName consCon  = '(:)
toName n | fromName n == fromName trueCon  = 'True
toName n | fromName n == fromName falseCon = 'False
toName n = TH.mkName $ fromName n
