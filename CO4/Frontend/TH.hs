{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- |Template Haskell front end
module CO4.Frontend.TH
  (parseTHDeclaration, module CO4.Frontend)
where

import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Util (programFromDeclarations)
import           CO4.Frontend
import           CO4.Names
import           CO4.Frontend.THCheck (check)
import           CO4.Frontend.THPreprocess (preprocess)

instance ProgramFrontend [TH.Dec] where
  parseProgram decs = 
    if check decs then parseTHDeclarations decs 
    else error "Frontend.TH.parseProgram: check failed"

  parsePreprocessedProgram decs = 
    if check decs then preprocess decs >>= return . parseTHDeclarations
    else error "Frontend.TH.parsePreprocessedProgram: check failed"

instance ExpressionFrontend TH.Exp where
  parseExpression exp = 
    if check exp then parseTHExpression exp
    else error "Frontend.TH.parseExpression: check failed"

  parsePreprocessedExpression exp =
    if check exp then preprocess exp >>= return . parseTHExpression
    else error "Frontend.TH.parsePreprocessedExpression: check failed"

{-
instance SchemeFrontend TH.Type where
  parseScheme type_ = parseTHType type_
-}

parseTHDeclarations :: [TH.Dec] -> Program
parseTHDeclarations = programFromDeclarations . map parseTHDeclaration

parseTHDeclaration :: TH.Dec -> Declaration
parseTHDeclaration dec = case dec of
  TH.FunD n [TH.Clause ps (TH.NormalB e) []] -> 
    DBind $ Binding (fromTHName n) (parseTHExpression $ TH.LamE ps e)

  TH.ValD (TH.VarP n) (TH.NormalB e) [] -> 
    DBind $ Binding (fromTHName n) (parseTHExpression e)

  TH.DataD [] n tVars cons _ -> -- ignores deriving statements
    let n'     = untypedName $ fromTHName n
        tVars' = map fromTHTyVarBndr tVars
        cons'  = map parseTHConstructor cons
    in
      DAdt n' tVars' cons'

  _ -> notSupported "parseTHDeclaration" dec

parseTHConstructor :: TH.Con -> Constructor
parseTHConstructor con = case con of
  TH.NormalC n strictTypes -> 
    CCon (untypedName $ fromTHName n) $ map parseTHStrictType strictTypes

  _ -> notSupported "parseTHConstructor" con

  where parseTHStrictType (_,t) = 
          let SType t' = parseTHType t
          in
            t'

parseTHExpression :: TH.Exp -> Expression
parseTHExpression expression = case expression of
  TH.VarE n | n == 'undefined           -> EUndefined
  TH.VarE n | fromName n == "undefined" -> EUndefined
  TH.VarE n                             -> EVar $ fromTHName n

  TH.ConE n -> ECon $ fromTHName n

  TH.AppE a b ->
    let (f,args) = gatherApplication a b
    in
      EApp (parse f) (map parse args)
  TH.LamE ps e     -> 
    let names' = map (\(TH.VarP n) -> fromTHName n) ps
    in
      ELam names' $ parse e
       
  TH.LetE decls e -> 
    let bindings = map (\d -> case parseTHDeclaration d of DBind b -> b) decls
    in
      ELet bindings $ parse e

  TH.CaseE e matches -> ECase (parse e) $ map parseTHMatch matches

  TH.LitE (TH.IntegerL i) -> ECon $ readName $ show i

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
  TH.VarP n       -> PVar $ fromTHName n
  TH.ConP n ps    -> PCon (fromTHName n) $ map parse ps
  TH.InfixP a n b -> PCon (fromTHName n) $ map parse [a,b]
  _               -> notSupported "parseTHPattern" pattern

  where parse = parseTHPattern

parseTHType :: TH.Type -> Scheme
parseTHType type_ = case type_ of
  TH.ForallT vs [] t -> foldr SForall (SType $ parse t) $ map fromTHTyVarBndr vs
  _                  -> SType $ parse type_

  where
    parse type_ = case type_ of
      TH.VarT v   -> TVar $ untypedName $ fromTHName v
      TH.ConT c   -> TCon (untypedName $ fromTHName c) []
      TH.ListT    -> TCon listName []
      TH.AppT a b -> case gatherApplication a b of
        (TH.ArrowT, args) -> 
          foldr1 (\arg result -> TCon funName [arg,result]) 
            $ map parse args

        (f, args) -> case parse f of
                        TCon c [] -> TCon c $ map parse args
                        TVar v    -> TCon v $ map parse args

      _           -> notSupported "parseTHType" type_

    gatherApplication f e =
      let gatherApplication' (TH.AppT f' e') es = gatherApplication' f' (e':es)
          gatherApplication' f' es              = (f',es)
      in
       gatherApplication' f [e]

fromTHTyVarBndr :: TH.TyVarBndr -> UntypedName
fromTHTyVarBndr bndr = case bndr of
  TH.PlainTV n -> untypedName $ fromTHName n
  _            -> notSupported "fromTHTyVarBndr" bndr

fromTHName :: TH.Name -> Name
fromTHName = name . TH.nameBase 

notSupported :: (TH.Ppr a, Show a) => String -> a -> b
notSupported funName a = 
  error $ concat ["Frontend.TH.", funName, ": '", show $ TH.ppr a, "' not supported (", show a,")"]
