{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module CO4.Algorithms.HindleyMilner.Util
  ( Context (..), Substitution, Free (..), Substitutable (..), mappend
  , substitutes, binds, bindTypes, generalize, generalizeAll, gamma
  , emptyContext, lookup, unsafeLookup, hasScheme, toList, instantiateSchemeApp
  , unifyOrFail, unifyLeftOrFail, unifiesOrFail)
where

import           Prelude hiding (lookup)
import           Control.Monad.Reader
import           Text.PrettyPrint ((<+>),hsep,text,nest,vcat,brackets)
import qualified Data.Map as M
import           Data.List ((\\),nub)
import           Data.Monoid (Monoid(..))
import           CO4.Algorithms.Free (Free(..))
import           CO4.Algorithms.Instantiator
import           CO4.Language 
import           CO4.PPrint (PPrint (..))

-- |A context is a mapping from names to schemes
data Context = Gamma (M.Map Name Scheme) deriving Show

type Substitution  = (Name,Type)

class Substitutable a where
  substitute :: Substitution -> a -> a

instance PPrint [Substitution] where
  pprint substs = brackets $ vcat $ map pprint substs

instance Substitutable Type where
  substitute (n,t') (TVar v) | v == n = t'
  substitute _      (TVar v)          = TVar v
  substitute s      (TCon n ts)       = TCon n $ map (substitute s) ts

instance Substitutable Scheme where
  substitute s     (SType t)      = SType $ substitute s t
  substitute (n,t) (SForall f x) = 
    if n == f then  SForall f x
              else  SForall f $ substitute (n,t) x

instance Free Context where
  free (Gamma c) = nub $ M.fold (\p -> (++) (free p)) [] c

instance Substitutable Context where
  substitute s (Gamma c) = Gamma $ M.map (substitute s) c

instance Monoid Context where
  mempty                      = gamma []
  mappend (Gamma a) (Gamma b) = Gamma $ M.union b a -- Needs to be right-biased

instance PPrint Context where
  pprint (Gamma ctxt) = 
    text "Gamma {" <+> nest 2 (vcat $ map ppBinding $ M.toList ctxt) <+> text "}"
    where
      ppBinding (name,scheme) = hsep [pprint name, text "::", pprint scheme]

instance Substitutable Name where
  substitute s (TypedName n t) = TypedName n $ substitute s t
  substitute _ name            = name

-- Expression substitution is done by an Instantiator monad
newtype Substituter a = Substituter { runSubstituter :: Reader Substitution a }
  deriving (Functor, Monad, MonadReader Substitution)

instance MonadInstantiator Substituter where
  instantiateScheme scheme = do
    subst <- ask
    return $ substitute subst scheme

  instantiateType t = do
    subst <- ask
    return $ substitute subst t

instance Substitutable Expression where
  substitute s exp = runReader (runSubstituter $ instantiateExpression exp) s

instance Substitutable Declaration where
  substitute s decl = runReader (runSubstituter $ instantiateDeclaration decl) s

instance Substitutable a => Substitutable [a] where
  substitute s as = map (substitute s) as

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  substitute s (a,b) = (substitute s a, substitute s b)

substitutes :: Substitutable a => [Substitution] -> a -> a
substitutes ss a = foldl (flip substitute) a ss

binds :: [(Name,Scheme)] -> Context -> Context
binds bindings context = mappend context $ gamma bindings

bindTypes :: [(Name,Type)] -> Context -> Context
bindTypes bindings context = 
  mappend context $ gamma $ map (\(n,t) -> (n, SType t)) bindings

generalize :: Context -> Type -> Scheme
generalize context t =
  let ts = (free t) \\ (free context)
  in
    foldl (flip SForall) (SType t) ts

generalizeAll :: Type -> Scheme
generalizeAll = generalize emptyContext

gamma :: [(Name,Scheme)] -> Context
gamma = Gamma . M.fromList

emptyContext :: Context
emptyContext = Gamma M.empty

lookup :: Name -> Context -> Maybe Scheme
lookup name (Gamma ctxt) = M.lookup name ctxt

unsafeLookup :: Name -> Context -> Scheme
unsafeLookup name context = case lookup name context of
  Nothing -> error $ "HindleyMilner.Util: unsafeLookup of '" ++ show (pprint name) ++ "'"
  Just s  -> s

hasScheme :: Name -> Context -> Bool
hasScheme name (Gamma ctxt) = M.member name ctxt

toList :: Context -> [(Name,Scheme)]
toList (Gamma context) = M.toList context

-- |Instantiates scheme by applying a list of types.
-- Note that this is no instantiation in terms of Hindley-Milners type inferrence.
instantiateSchemeApp :: Scheme -> [Type] -> Scheme
instantiateSchemeApp = 
  let instantiate scheme t' = case scheme of
        SForall n s -> substitute (n,t') s
        SType t     -> case free t of
                        []    -> SType t
                        (f:_) -> SType $ substitute (f,t') t
  in
    foldl instantiate


-- |Unifies two types or fails if there is no substitution
unifyOrFail, unifyLeftOrFail  :: Monad m => Type -> Type -> m [Substitution]
unifyOrFail t1 t2 = case unify False t1 t2 of
  Left msg -> fail msg
  Right s  -> return s

unifyLeftOrFail t1 t2 = case unify True t1 t2 of
  Left msg -> fail msg
  Right s  -> return s

unifiesOrFail :: Monad m => [Type] -> [Type] -> m [Substitution]
unifiesOrFail ts1 ts2 = case unifies False ts1 ts2 of
  Left msg -> fail msg
  Right s  -> return s

-- |@unify a b@ unifies the types @a@ and @b@. 
unify :: Bool -> Type -> Type -> Either String [Substitution]
unify left t1 t2 = case (t1,t2) of
  (TVar v, _)   -> return [(v, t2)]
  (_  , TVar v) -> if left then Left noLeftUnifierFound
                   else return [(v, t1)]
   
  (TCon c1 ts1, TCon c2 ts2) -> 
     if c1 == c2 && (length ts1 == length ts2)
     then unifies left ts1 ts2
     else Left noUnifierFound
  
  --_ -> Left noUnifierFound
  
  where 
    noUnifierFound = "No unifier found for '" ++ show (pprint t1) ++ "' and '" ++ show (pprint t2) ++ "'"

    noLeftUnifierFound = "Provided type '" ++ show (pprint t2) ++ "' is more general than inferred type '" ++ show (pprint t1) ++ "'"

-- | Unifies two lists of types 
unifies :: Bool -> [Type] -> [Type] -> Either String [Substitution]
unifies left ts1 ts2 =
  if (length ts1) /= (length ts2) 
  then Left $ "Different lengths of '" ++ show (map pprint ts1) ++ "' and '" ++ show (map pprint ts2) ++ "'"
  else 
    foldM (\s (a,b) -> do s' <- unify left (substitutes s a) (substitutes s b)
                          return $ s ++ s' 
          ) [] $ zip ts1 ts2

