-- |Template Haskell preprocessing
module CO4.Frontend.THPreprocess
  (preprocess, eraseDerivings)
where

import           Control.Monad (liftM)
import           Control.Exception (assert)
import qualified Data.Map as M
import           Data.Generics (GenericM,GenericT,everywhere,everywhere',everywhereM
                               ,mkM,mkT)
import           Language.Haskell.TH 
import           CO4.Unique (MonadUnique,newString)
import           CO4.THUtil (deleteSignatures,deleteTypeSynonyms)
import           CO4.Names (consName,listName,tupleName)

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
  >>=          everywhereM (mkM noWildcardPattern)
  >>=          everywhereM (mkM noNestedPatternsInClauseParameters)
  >>=          everywhereM (mkM noNestedPatternsInLambdaParameters)
  >>= return . everywhere  (mkT noListPattern) 
  >>= return . everywhere  (mkT noListExpression) 
  >>= return . everywhere  (mkT noTuplePattern) 
  >>= return . everywhere  (mkT noTupleExpression) 
  >>= return .                  deleteSignatures
  >>= return . everywhere  (mkT expandTypeSynonyms)
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

-- |Transforms nested patterns in arguments of function clauses into case expression
-- over those arguments
noNestedPatternsInClauseParameters :: MonadUnique u => Clause -> u Clause
noNestedPatternsInClauseParameters (Clause ps (NormalB e) d) = do
  (ps',e') <- noNestedPatterns ps e
  return $ Clause ps' (NormalB e') d

noNestedPatternsInLambdaParameters :: MonadUnique u => Exp -> u Exp
noNestedPatternsInLambdaParameters exp = case exp of
  LamE ps e -> do
    (ps',e') <- noNestedPatterns ps e
    return $ LamE ps' e'
  _ -> return exp

-- |Removes wildcard patterns by introducting new pattern variables
noWildcardPattern :: MonadUnique u => Pat -> u Pat
noWildcardPattern pat = case pat of
  WildP -> liftM VarP $ newTHName "wildcard"
  _     -> return pat

noNestedPatterns :: MonadUnique u => [Pat] -> Exp -> u ([Pat],Exp)
noNestedPatterns [VarP p] exp = return ([VarP p],exp)
noNestedPatterns [p]      exp = do
  n <- newTHName "noVar"
  return ([VarP n], CaseE (VarE n) [Match p (NormalB exp) []])
noNestedPatterns (p:ps)   exp = do
  (ps',e') <- noNestedPatterns ps  exp
  (p',e'') <- noNestedPatterns [p] e'
  return (p' ++ ps', e'')

noListPattern :: Pat -> Pat
noListPattern pat = case pat of
  ListP xs -> foldr (\x y -> ConP consName [x,y]) (ConP listName []) xs
  _        -> pat

noListExpression :: Exp -> Exp
noListExpression exp = case exp of
  ListE xs -> foldr (\x -> AppE $ AppE (ConE consName) x) (ConE listName) xs
  _        -> exp

noTuplePattern :: Pat -> Pat
noTuplePattern pat = case pat of
  TupP xs -> ConP (tupleName $ length xs) xs
  _       -> pat

noTupleExpression :: Exp -> Exp
noTupleExpression exp = case exp of
  TupE xs -> foldl AppE (ConE $ tupleName $ length xs) xs
  _       -> exp

expandTypeSynonyms :: [Dec] -> [Dec]
expandTypeSynonyms decs = everywhere' (mkT expand) decs
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

newTHName :: MonadUnique u => String -> u Name
newTHName name = liftM mkName $ newString name
