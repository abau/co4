-- |Template Haskell preprocessing
module CO4.Frontend.THPreprocess
  (preprocess, eraseDerivings)
where

import           Control.Monad (liftM)
import           Data.Generics (GenericM,GenericT,everywhere,everywhereM,mkM,mkT)
import           Language.Haskell.TH 
import           CO4.Unique (MonadUnique,newString)
import           CO4.THUtil (deleteSignatures,deleteTypeSynonyms)
  
-- |Performs preprocessing on TH's AST 
preprocess :: MonadUnique u => GenericM u
preprocess a = everywhereM (mkM noMultipleClauses) a
  >>= return . everywhere  (mkT noWhereInDec) 
  >>= return . everywhere  (mkT noWhereInMatch) 
  >>= return . everywhere  (mkT noWhereInClause) 
  >>=          everywhereM (mkM noInfixApp)
  >>= return . everywhere  (mkT noParensExpression) 
  >>= return . everywhere  (mkT noInfixPattern) 
  >>= return . everywhere  (mkT noParensPattern) 
  >>=          everywhereM (mkM noComplexPatternsInClauseParameters)
  >>=          everywhereM (mkM noComplexPatternsInLambdaParameters)
  >>=          everywhereM (mkM noWildcardPattern)
  >>= return .                  deleteSignatures
  >>= return .                  deleteTypeSynonyms

-- |Erases instance derivings from data declarations and newtype declarations
eraseDerivings :: GenericT
eraseDerivings = everywhere $ mkT go
  where 
    go (DataD    ctxt name tvbs cons _) = DataD    ctxt name tvbs cons []
    go (NewtypeD ctxt name tvbs cons _) = NewtypeD ctxt name tvbs cons []
    go dec                              = dec

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

-- |Transforms 'where' clauses into let expressions
noWhereInDec :: Dec -> Dec
noWhereInDec dec = case dec of
  ValD _ _ []          -> dec
  ValD pat body where_ -> ValD pat (mapBodyExps (LetE where_) body) []
  _                    -> dec

noWhereInMatch :: Match -> Match
noWhereInMatch (Match pat body where_) = case where_ of
  [] -> Match pat body []
  _  -> Match pat (mapBodyExps (LetE where_) body) []

noWhereInClause :: Clause -> Clause
noWhereInClause (Clause pats body where_) = case where_ of
  [] -> Clause pats body []
  _  -> Clause pats (mapBodyExps (LetE where_) body) []

mapBodyExps :: (Exp -> Exp) -> Body -> Body
mapBodyExps f body = case body of
  NormalB e   -> NormalB $ f e
  GuardedB gb -> GuardedB $ mapGuardedBodies gb
    where
      mapGuardedBodies = map (\(guard,e) -> (guard, f e))


-- |Transforms infix applications to prefix applications
noInfixApp :: MonadUnique u => Exp -> u Exp
noInfixApp exp = case exp of
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
  UInfixE a op b  -> noInfixApp $ InfixE (Just a) op (Just b)
  _ -> return exp

-- |Removes parens expressions
noParensExpression :: Exp -> Exp
noParensExpression e = case e of
  ParensE e -> e
  _         -> e

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

-- |Transforms complex patterns in arguments of function clauses into case expression
-- over those arguments
noComplexPatternsInClauseParameters :: MonadUnique u => Clause -> u Clause
noComplexPatternsInClauseParameters (Clause ps (NormalB e) d) = do
  (ps',e') <- noComplexPatterns ps e
  return $ Clause ps' (NormalB e') d

noComplexPatternsInLambdaParameters :: MonadUnique u => Exp -> u Exp
noComplexPatternsInLambdaParameters exp = case exp of
  LamE ps e -> do
    (ps',e') <- noComplexPatterns ps e
    return $ LamE ps' e'
  _ -> return exp

noComplexPatterns :: MonadUnique u => [Pat] -> Exp -> u ([Pat],Exp)
noComplexPatterns [VarP p] exp = return ([VarP p],exp)
noComplexPatterns [p]      exp = do
  n <- newTHName "noVar"
  return ([VarP n], CaseE (VarE n) [Match p (NormalB exp) []])
noComplexPatterns (p:ps)   exp = do
  (ps',e') <- noComplexPatterns ps  exp
  (p',e'') <- noComplexPatterns [p] e'
  return (p' ++ ps', e'')

-- |Removes wildcard patterns by introducting new pattern variables
noWildcardPattern :: MonadUnique u => Pat -> u Pat
noWildcardPattern pat = case pat of
  WildP -> liftM VarP $ newTHName "wildcard"
  _     -> return pat

newTHName :: MonadUnique u => String -> u Name
newTHName name = liftM mkName $ newString name
