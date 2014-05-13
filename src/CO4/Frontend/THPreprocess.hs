-- |Template Haskell preprocessing
module CO4.Frontend.THPreprocess
  ( preprocessDecs, noSignatureExpression, noSignaturePattern, noSignatureDeclarations)
where

import           Control.Monad (liftM)
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Generics (everywhere,everywhereM,mkT,mkM)
import           Language.Haskell.TH 
import           CO4.Unique (MonadUnique,newString)
import           CO4.Names (nilName,consName)
import           CO4.Util (extM')

type TypeSynonyms = M.Map Name ([TyVarBndr], Type)

preprocessDecs :: MonadUnique u => [Dec] -> u [Dec]
preprocessDecs decs = everywhereM 
                    ( extM' (return . onDecs)
                    $ extM' onPat 
                    $ extM' onMatch
                    $ extM' onClause
                    $ extM' (return . onExpandedType synonyms)
                    $ mkM   onExp
                    ) decs
  where
    synonyms = M.fromList $ concatMap synonym decs

    synonym (TySynD name vars t) = 
      let t' = everywhere (mkT onType) t
      in
        [(name, (vars,t'))]
    synonym _                    = []

onExp :: MonadUnique u => Exp -> u Exp
onExp a = noInfixAppExpression a >>= noNestedPatternsInLambdaParameters 
      >>= return . noParensExpression . noListExpression . noTupleExpression
                 . noSignatureExpression

onPat :: MonadUnique u => Pat -> u Pat
onPat a = noWildcardPattern a
      >>= return . noParensPattern . noInfixPattern . noListPattern . noTuplePattern
                 . noSignaturePattern

onMatch :: MonadUnique u => Match -> u Match
onMatch = noNestedPatternsInMatch

onClause :: MonadUnique u => Clause -> u Clause
onClause = noNestedPatternsInClauseParameters

onExpandedType :: TypeSynonyms -> Type -> Type
onExpandedType synonyms = expandTypeSynonyms synonyms . onType

onType :: Type -> Type
onType = noTupleType

onDecs :: [Dec] -> [Dec]
onDecs = noTypeSynonyms

-- Preprocessors on `Exp` ------------------------------------------------

-- |Transforms infix applications to prefix applications
noInfixAppExpression :: MonadUnique u => Exp -> u Exp
noInfixAppExpression exp = case exp of
  InfixE (Just a) op (Just b) -> return $ AppE (AppE op a) b
  InfixE (Just a) op Nothing  -> 
    do u <- newTHName "noInfixApp"
       return $ LamE [VarP u] $ AppE (AppE op a) $ VarE u
  InfixE Nothing op (Just b)  -> 
    do u <- newTHName "noInfixApp"
       return $ LamE [VarP u] $ AppE (AppE op $ VarE u) b
  InfixE Nothing op Nothing  -> 
    do u <- newTHName "noInfixApp"
       v <- newTHName "noInfixApp"
       return $ LamE [VarP u,VarP v] $ AppE (AppE op $ VarE u) $ VarE v
  UInfixE a op b  -> noInfixAppExpression $ InfixE (Just a) op (Just b)
  _ -> return exp

-- |Removes parens expressions
noParensExpression :: Exp -> Exp
noParensExpression e = case e of
  ParensE e -> e
  _         -> e

noListExpression :: Exp -> Exp
noListExpression exp = case exp of
  ListE xs -> foldr (\x -> AppE $ AppE (ConE consName) x) (ConE nilName) xs
  _        -> exp

noTupleExpression :: Exp -> Exp
noTupleExpression exp = case exp of
  TupE xs -> foldl AppE (ConE $ tupleDataName $ length xs) xs
  _       -> exp

noSignatureExpression :: Exp -> Exp
noSignatureExpression exp = case exp of 
  SigE e _ -> e
  _        -> exp

-- |Transforms nested patterns in arguments of lambda expressions into case expressions
-- over those arguments
noNestedPatternsInLambdaParameters :: MonadUnique u => Exp -> u Exp
noNestedPatternsInLambdaParameters exp = case exp of
  LamE ps e -> do
    (ps',e') <- onlyVariablePatterns ps e
    return $ LamE ps' e'
  _ -> return exp

-- Preprocessors on `Pat` ------------------------------------------------

-- |Removes parens patterns
noParensPattern :: Pat -> Pat
noParensPattern p = case p of
  ParensP p -> p
  _         -> p

-- |Transforms infix patterns to contructor patterns
noInfixPattern :: Pat -> Pat
noInfixPattern pat = case pat of
  InfixP p1 n p2  -> ConP n [p1,p2]
  UInfixP p1 n p2 -> noInfixPattern $ InfixP p1 n p2
  _               -> pat

-- |Removes wildcard patterns by introducting new pattern variables
noWildcardPattern :: MonadUnique u => Pat -> u Pat
noWildcardPattern pat = case pat of
  WildP -> liftM VarP $ newTHName "wildcard"
  _     -> return pat

noListPattern :: Pat -> Pat
noListPattern pat = case pat of
  ListP xs -> foldr (\x y -> ConP consName [x,y]) (ConP nilName []) xs
  _        -> pat

noTuplePattern :: Pat -> Pat
noTuplePattern pat = case pat of
  TupP xs -> ConP (tupleDataName $ length xs) xs
  _       -> pat

noSignaturePattern :: Pat -> Pat
noSignaturePattern pat = case pat of 
  SigP p _ -> p
  _        -> pat

-- Preprocessors on `Match` ------------------------------------------------

-- |Transforms nested patterns in matches into case expressions over those arguments
noNestedPatternsInMatch :: MonadUnique u => Match -> u Match
noNestedPatternsInMatch (Match pat (NormalB body) decs) = do
  (pat',body') <- noNestedPatterns pat body
  return $ Match pat' (NormalB body') decs

  where 
    noNestedPatterns (VarP p) exp    = return (VarP p, exp)
    noNestedPatterns (ConP c ps) exp = do
      (ps',exp') <- onlyVariablePatterns ps exp
      return (ConP c ps', exp')

-- Preprocessors on `Clause` ------------------------------------------------

-- |Transforms nested patterns in arguments of function clauses into case expressions
-- over those arguments
noNestedPatternsInClauseParameters :: MonadUnique u => Clause -> u Clause
noNestedPatternsInClauseParameters (Clause ps (NormalB e) d) = do
  (ps',e') <- onlyVariablePatterns ps e
  return $ Clause ps' (NormalB e') d

-- Preprocessors on `Type` ------------------------------------------------

noTupleType :: Type -> Type
noTupleType ty = case ty of
  TupleT i -> ConT (tupleTypeName i)
  _        -> ty

expandTypeSynonyms :: TypeSynonyms -> Type -> Type
expandTypeSynonyms synonyms t = case collectAppT t of
  (ConT con):args -> case M.lookup con synonyms of
    Nothing        -> t
    Just (vars,t') -> if length args < length vars 
                      then t
                      else 
                        let expanded = foldl' apply t' $ zip vars args
                        in
                          everywhere (mkT $ expandTypeSynonyms synonyms) expanded
  _ -> t
  where
    collectAppT (AppT a b) = (collectAppT a) ++ [b]
    collectAppT t          = [t]

    apply t (PlainTV v,t') = everywhere (mkT applyInType) t
      where
        applyInType (VarT v') | v == v' = t'
        applyInType x                   = x

-- Preprocessors on `[Dec]` ------------------------------------------------

noSignatureDeclarations :: [Dec] -> [Dec]
noSignatureDeclarations = filter (not . isSig)
  where
    isSig (SigD {}) = True
    isSig _         = False

noTypeSynonyms :: [Dec] -> [Dec]
noTypeSynonyms = filter (not . isSynonym)
  where
    isSynonym (TySynD {}) = True
    isSynonym _           = False

-- Utilities ------------------------------------------------------------------

onlyVariablePatterns :: MonadUnique u => [Pat] -> Exp -> u ([Pat],Exp)
onlyVariablePatterns [] exp       = return ([],exp)
onlyVariablePatterns [VarP p] exp = return ([VarP p],exp)
onlyVariablePatterns [p]      exp = do
  n <- newTHName "varPat"
  return ([VarP n], CaseE (VarE n) [Match p (NormalB exp) []])
onlyVariablePatterns (p:ps)   exp = do
  (ps',e') <- onlyVariablePatterns ps  exp
  (p',e'') <- onlyVariablePatterns [p] e'
  return (p' ++ ps', e'')

newTHName :: MonadUnique u => String -> u Name
newTHName name = liftM mkName $ newString name
