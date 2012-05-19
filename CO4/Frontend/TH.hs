{-# LANGUAGE FlexibleInstances #-}
-- |Template Haskell front end
module CO4.Frontend.TH
  ()
where

import qualified Data.Map as M
import           Data.List (partition)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Frontend
import           CO4.Names
import           CO4.Frontend.THCheck (check)
import           CO4.Frontend.THPreprocess (preprocess)

instance ProgramFrontend [TH.Dec] where
  parseProgram decs = do
    check decs
    decs' <- preprocess decs
    return $ 
      let (signatures,rest) = partition isSignature decs'
          signatures'       = signatureMap signatures
          rest'             = map parseTHDeclaration rest
      in
        map (\(DBind n v) -> case M.lookup n signatures' of 
                              Nothing -> DBind n v
                              Just s  -> DBind (nTyped n s) v
            ) rest'
    where
      isSignature (TH.SigD {}) = True
      isSignature _            = False

      signatureMap = 
        foldr (\(TH.SigD n t) -> M.insert (fromTHName n) $ parseTHType t) M.empty
  
instance ExpressionFrontend TH.Exp where
  parseExpression exp = do
    check exp
    exp' <- preprocess exp
    return $ parseTHExpression exp'

instance SchemeFrontend TH.Type where
  parseScheme type_ = parseTHType type_

parseTHDeclaration :: TH.Dec -> Declaration
parseTHDeclaration dec = case dec of
  TH.FunD n [TH.Clause ps (TH.NormalB e) []] -> 
    DBind (fromTHName n) (parseTHExpression $ TH.LamE ps e)

  TH.ValD (TH.VarP n) (TH.NormalB e) [] -> DBind (fromTHName n) 
                                                 (parseTHExpression e)

  _ -> notSupported "parseTHDeclaration" dec

parseTHExpression :: TH.Exp -> Expression
parseTHExpression expression = case expression of
  TH.VarE n              -> EVar $ fromTHName n
  TH.ConE n              -> ECon $ fromTHName n
  TH.LitE (TH.StringL s) -> parse $ TH.ListE $ map (TH.LitE . TH.CharL) s
  TH.LitE l              -> ELit $ parseTHLiteral l
  TH.AppE a b ->
    let (f,args) = gatherApplication a b
    in
      EApp (parse f) (map parse args)
  TH.LamE ps e     -> 
    let names' = map (\(TH.VarP n) -> fromTHName n) ps
    in
      ELam names' $ parse e
       
  TH.TupE es       -> EApp (ECon (tupleCon $ length es)) $ map parse es
  TH.CondE c p n   -> 
    ECase (parse c) [ Match (PCon trueCon  []) $ parse p
                    , Match (PCon falseCon []) $ parse n
                    ]
  TH.LetE [] e     -> parse e
  TH.LetE (x:xs) e -> 
    let DBind n e' = parseTHDeclaration x
    in
      ELet n e' $ parse $ TH.LetE xs e

  TH.CaseE e matches -> ECase (parse e) $ map parseTHMatch matches
  TH.ListE xs        -> foldr (\x rest -> EApp (ECon consCon) [x,rest]) 
                          (ECon nilCon) $ map parse xs

  _ -> notSupported "parseTHExpression" expression
  
  where
    parse = parseTHExpression

    gatherApplication f e =
      let gatherApplication' (TH.AppE f' e') es = gatherApplication' f' (e':es)
          gatherApplication' f' es              = (f',es)
      in
       gatherApplication' f [e]

parseTHMatch :: TH.Match -> Match
parseTHMatch (TH.Match p (TH.NormalB e) []) = 
  Match (parseTHPattern p) $ parseTHExpression e

parseTHPattern :: TH.Pat -> Pattern
parseTHPattern pattern = case pattern of
  TH.LitP l       -> 
    case l of TH.StringL s -> parse $ TH.ListP $ map (TH.LitP . TH.CharL) s
              _            -> PLit $ parseTHLiteral l
  TH.VarP n       -> PVar $ fromTHName n
  TH.TupP ts      -> PCon (tupleCon $ length ts) $ map parse ts
  TH.ConP n ps    -> PCon (fromTHName n) $ map parse ps
  TH.InfixP a n b -> PCon (fromTHName n) $ map parse [a,b]
  TH.ListP ps     -> foldr (\x rest -> PCon consCon [x,rest]) 
                      (PCon nilCon []) $ map parse ps
  _               -> notSupported "parseTHPattern" pattern

  where parse = parseTHPattern

parseTHLiteral :: TH.Lit -> Literal
parseTHLiteral literal = case literal of
  TH.CharL l       -> LChar l
  TH.IntegerL l    -> LInt l
  TH.RationalL l   -> LDouble $ fromRational l
  _                -> notSupported "parseTHLiteral" $ TH.LitE literal

parseTHType :: TH.Type -> Scheme
parseTHType type_ = case type_ of
  TH.ForallT vs [] t -> foldr SForall (SType $ parse t) $ map fromTHTyVarBndr vs
  _                  -> SType $ parse type_

  where
    parse type_ = case type_ of
      TH.VarT v   -> TVar $ untypedName $ fromTHName v
      TH.ConT c   -> TCon (untypedName $ fromTHName c) []
      TH.TupleT i -> TCon (untypedName $ tupleType i) []
      TH.AppT a b -> case gatherApplication a b of
        (TH.ArrowT, args) -> 
          foldr1 (\arg result -> TCon (untypedName funType) [arg,result]) 
            $ map parse args

        (TH.ListT, args) -> TCon (untypedName listType) $ map parse args

        (f, args) -> case parse f of
                        TCon c [] -> TCon c $ map parse args
                        TVar v    -> TCon v $ map parse args

      _           -> notSupported "parseTHType" type_

    fromTHTyVarBndr bndr = case bndr of
      TH.PlainTV n -> untypedName $ fromTHName n
      _            -> notSupported "fromTHTyVarBndr" bndr

    gatherApplication f e =
      let gatherApplication' (TH.AppT f' e') es = gatherApplication' f' (e':es)
          gatherApplication' f' es              = (f',es)
      in
       gatherApplication' f [e]

fromTHName :: TH.Name -> Name
fromTHName thName = case TH.nameBase thName of
  ":"   -> consCon
  "[]"  -> nilCon
  other -> name other

notSupported :: (TH.Ppr a, Show a) => String -> a -> b
notSupported funName a = 
  error $ concat ["Frontend.TH.", funName, ": '", show $ TH.ppr a, "' not supported", show a]
