-- |Template Haskell preprocessing
module CO4.Frontend.THPreprocess
  (preprocess)
where

import           Control.Applicative ((<$>))
import           Data.Generics (GenericM,everywhere,everywhereM,mkM,mkT)
import           Language.Haskell.TH 
import           CO4.Unique (Unique,newString)
  
-- |Performs preprocessing on TH's AST 
preprocess :: GenericM Unique
preprocess a = everywhereM (mkM noMultipleClauses) a
  >>= return . everywhere  (mkT noWhereInDec) 
  >>= return . everywhere  (mkT noWhereInMatch) 
  >>= return . everywhere  (mkT noWhereInClause) 
  >>=          everywhereM (mkM noInfixApp)
  >>= return . everywhere  (mkT noInfixPattern) 
  >>=          everywhereM (mkM noComplexPatternsInClauseParameters)
  >>=          everywhereM (mkM noComplexPatternsInLambdaParameters)
  >>=          everywhereM (mkM noWildcardPattern)

-- |Transforms multiple clauses of a function declaration into a single declaration
-- with a case expression as outermost expression
noMultipleClauses :: [Clause] -> Unique [Clause]
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
noInfixApp :: Exp -> Unique Exp
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
  _ -> return exp

-- |Transforms infix patterns to contructor patterns
noInfixPattern :: Pat -> Pat
noInfixPattern pat = case pat of
  InfixP p1 n p2 -> ConP n [p1,p2]
  _              -> pat

-- |Transforms complex patterns in arguments of function clauses into case expression
-- over those arguments
noComplexPatternsInClauseParameters :: Clause -> Unique Clause
noComplexPatternsInClauseParameters (Clause ps (NormalB e) d) = do
  (ps',e') <- noComplexPatterns ps e
  return $ Clause ps' (NormalB e') d

noComplexPatternsInLambdaParameters :: Exp -> Unique Exp
noComplexPatternsInLambdaParameters exp = case exp of
  LamE ps e -> do
    (ps',e') <- noComplexPatterns ps e
    return $ LamE ps' e'
  _ -> return exp

noComplexPatterns :: [Pat] -> Exp -> Unique ([Pat],Exp)
noComplexPatterns [VarP p] exp = return ([VarP p],exp)
noComplexPatterns [p]      exp = do
  n <- newTHName "noVar"
  return ([VarP n], CaseE (VarE n) [Match p (NormalB exp) []])
noComplexPatterns (p:ps)   exp = do
  (ps',e') <- noComplexPatterns ps  exp
  (p',e'') <- noComplexPatterns [p] e'
  return (p' ++ ps', e'')

noWildcardPattern :: Pat -> Unique Pat
noWildcardPattern pat = case pat of
  WildP -> VarP <$> newTHName "wildcard"
  _     -> return pat

newTHName :: String -> Unique Name
newTHName name = mkName <$> newString name
