-- |Template Haskell preprocessing
module CO4.Frontend.THPreprocess
  (preprocess)
where

import           Control.Monad (liftM)
import           Control.Exception (assert)
import qualified Data.Map as M
import           Data.Generics (GenericM,everywhere,everywhere',everywhereM,mkT,mkM,extM)
import           Language.Haskell.TH 
import           CO4.Unique (MonadUnique,newString)
import           CO4.THUtil (deleteSignatures,deleteTypeSynonyms)
import           CO4.Names (consName,listName,tupleName)

-- |Performs preprocessing on TH's AST 
preprocess :: MonadUnique u => GenericM u
preprocess a = everywhereM ( extM' onPat 
                           $ extM' onMatch
                           $ extM' onClause
                           $ extM' onClauses
                           $ extM' onType
                           $ extM' onDecs
                           $ mkM onExp
                           ) a
           >>= return . deleteTypeSynonyms . deleteSignatures
  where
    extM' a b = extM b a

    onExp a   = noInfixAppExpression a >>= noNestedPatternsInLambdaParameters 
            >>= return . noParensExpression . noListExpression . noTupleExpression

    onPat a   = noWildcardPattern a
            >>= return . noParensPattern . noInfixPattern . noListPattern . noTuplePattern

    onMatch   = noNestedPatternsInMatch

    onClause  = noNestedPatternsInClauseParameters

    onClauses = noMultipleClauses

    onType    = return . noTupleType

    onDecs    = return . expandTypeSynDeclarations

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
  ListE xs -> foldr (\x -> AppE $ AppE (ConE consName) x) (ConE listName) xs
  _        -> exp

noTupleExpression :: Exp -> Exp
noTupleExpression exp = case exp of
  TupE xs -> foldl AppE (ConE $ tupleName $ length xs) xs
  _       -> exp

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
  ListP xs -> foldr (\x y -> ConP consName [x,y]) (ConP listName []) xs
  _        -> pat

noTuplePattern :: Pat -> Pat
noTuplePattern pat = case pat of
  TupP xs -> ConP (tupleName $ length xs) xs
  _       -> pat

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

-- Preprocessors on `[Clause]` ------------------------------------------------

-- |Transforms multiple clauses of a function declaration into a single declaration
-- with a case expression as outermost expression
noMultipleClauses :: MonadUnique u => [Clause] -> u [Clause]
noMultipleClauses []      = return [] 
noMultipleClauses [x]     = return [x] 
noMultipleClauses clauses = 
  let numParams = case head clauses of Clause pats _ _ -> length pats
      clauseToMatch (Clause [p]  body decs) = Match p           body decs
      clauseToMatch (Clause pats body decs) = Match (TupP pats) body decs
    in do
      names <- mapM (const $ newTHName "noMultipleClauses") [1..numParams]
       
      let caseE = case names of [n] -> VarE n
                                _   -> TupE $ map VarE names
      
      return $ [Clause (map VarP names) (
                    NormalB $ CaseE caseE $ map clauseToMatch clauses) []
               ] 

-- Preprocessors on `Type` ------------------------------------------------

noTupleType :: Type -> Type
noTupleType ty = case ty of
  TupleT i -> ConT (tupleName i)
  _        -> ty

-- Preprocessors on `[Dec]` ------------------------------------------------

expandTypeSynDeclarations :: [Dec] -> [Dec]
expandTypeSynDeclarations decs = everywhere' (mkT expand) decs
  where
    synonyms = M.fromList $ concatMap synonym decs

    synonym (TySynD name vars t) = [(name,(vars,t))]
    synonym _                    = []

    expand t | M.null synonyms = t
    expand t                   = case collectAppT t of
      (ConT con):args -> case M.lookup con synonyms of
        Nothing        -> foldl1 AppT $ (ConT con) : (map expand args)
        Just (vars,t') -> assert (length vars == length args) $
                          expand $ foldl apply t' $ zip vars args
      _ -> t

    collectAppT (AppT a b) = (collectAppT a) ++ [b]
    collectAppT t          = [t]

    apply t (PlainTV v,t') = everywhere (mkT applyInType) t
      where
        applyInType (VarT v') | v == v' = t'
        applyInType x                   = x

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
