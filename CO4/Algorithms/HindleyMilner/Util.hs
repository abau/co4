{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Algorithms.HindleyMilner.Util
  ( Context (..), Substitution, Substitutable (..), mappend
  , substituteN, bind, bindTypes, bindAdt, bindTypedBindings
  , generalize, generalizeAll, gamma
  , emptyContext, lookup, unsafeLookup, hasScheme, toList, instantiateSchemeApp
  , unifyOrFail, unifyNorFail)
where

import           Prelude hiding (lookup)
import           Control.Monad.Reader
import           Text.PrettyPrint ((<+>),hsep,text,nest,vcat)
import qualified Data.Map.Strict as M
import           Data.List ((\\),nub)
import           Data.Monoid (Monoid(..))
import           CO4.Algorithms.Free (Free(..))
import           CO4.Algorithms.Instantiator
import           CO4.Language 
import           CO4.PPrint (PPrint (..))
import           CO4.Names (Namelike,untypedName)
import           CO4.TypesUtil (functionType,typeOfAdt)

-- |A context is a mapping from names to schemes
data Context = Gamma (M.Map UntypedName Scheme) deriving Show

type Substitution  = (UntypedName,Type)

class Substitutable a where
  substitute :: Substitution -> a -> a

instance Substitutable Scheme where
  substitute s     (SType t)                 = SType $ substitute s t
  substitute (n,_) (SForall f x) | n == f    = SForall f x
  substitute (n,t) (SForall f x)             = SForall f $ substitute (n,t) x

instance Substitutable Type where
  substitute (n,t') (TVar v) | v == n = t'
  substitute _      (TVar v)          = TVar v
  substitute s      (TCon n ts)       = TCon n $ map (substitute s) ts

instance Substitutable Name where
  substitute s (NTyped n t) = NTyped n $ substitute s t
  substitute _ name         = name

instance Free Context where
  free (Gamma c) = nub $ M.foldl' (\fs p -> fs ++ free p) [] c

instance Substitutable Context where
  substitute s (Gamma c) = Gamma $ M.map (substitute s) c

instance Monoid Context where
  mempty                      = emptyContext
  mappend (Gamma a) (Gamma b) = Gamma $ M.union b a -- Needs to be right-biased

instance PPrint Context where
  pprint (Gamma ctxt) = 
    text "Gamma {" <+> nest 2 (vcat $ map ppBinding $ M.toList ctxt) <+> text "}"
    where
      ppBinding (name,scheme) = hsep [pprint name, text "::", pprint scheme]

-- Expression substitution is done by an Instantiator monad
newtype Substituter a = Substituter { runSubstituter :: Reader Substitution a }
  deriving (Monad, MonadReader Substitution)

instance MonadInstantiator Substituter where
  instantiateScheme scheme = do
    subst <- ask
    return $ substitute subst scheme

  instantiateType t = do
    subst <- ask
    return $ substitute subst t

-- We would like to write @instance Instantiable i => Substitutable i@ but
-- this needs undecidable instances
instance Substitutable Expression where
  substitute s a = runReader (runSubstituter $ instantiate a) s

instance Substitutable Declaration where
  substitute s a = runReader (runSubstituter $ instantiate a) s

instance Substitutable Pattern where
  substitute s a = runReader (runSubstituter $ instantiate a) s

instance Substitutable a => Substitutable [a] where
  substitute s as = map (substitute s) as

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  substitute s (a,b) = (substitute s a, substitute s b)

substituteN :: Substitutable a => [Substitution] -> a -> a
substituteN ss a = foldl (flip substitute) a ss'
  where 
    ss' = filter (\case (n, TVar v) -> n /= v
                        _           -> True) ss

bind :: Namelike n => [(n,Scheme)] -> Context -> Context
bind bindings context = mappend context $ gamma bindings

bindSchemes :: Namelike n => [(n,Scheme)] -> Context -> Context
bindSchemes bindings context = mappend context $ gamma bindings

bindTypes :: Namelike n => [(n,Type)] -> Context -> Context
bindTypes bindings = bindSchemes $ map (\(n,t) -> (n, SType t)) bindings

bindAdt :: Adt -> Context -> Context
bindAdt adt context = foldr bindConstructor context $ adtConstructors adt
  where bindConstructor (CCon name types) = 
          let scheme = foldr SForall (SType $ functionType types $ typeOfAdt adt) 
                                     (adtTypeVariables adt)
          in
            bind [(name, scheme)]

bindTypedBindings :: [Binding] -> Context -> Context
bindTypedBindings b = bind (map toContextBinding b)
  where toContextBinding (Binding (NTyped n s) _) = (UntypedName n, s)

generalize :: Context -> Type -> Scheme
generalize context t =
  let ts = (free t) \\ (free context)
  in
    foldl (flip SForall) (SType t) $ map untypedName ts

generalizeAll :: Type -> Scheme
generalizeAll = generalize emptyContext

gamma :: Namelike n => [(n,Scheme)] -> Context
gamma = Gamma . M.fromList . map (\(n,t) -> (untypedName n, t))

emptyContext :: Context
emptyContext = Gamma M.empty

lookup :: Namelike n => n -> Context -> Maybe Scheme
lookup name (Gamma ctxt) = M.lookup (untypedName name) ctxt

unsafeLookup :: (Namelike n, PPrint n) => n -> Context -> Scheme
unsafeLookup name context = case lookup (untypedName name) context of
  Nothing -> error $ "HindleyMilner.Util: unsafeLookup of '" ++ show (pprint name) ++ "'"
  Just s  -> s

hasScheme :: Namelike n => n -> Context -> Bool
hasScheme name (Gamma ctxt) = M.member (untypedName name) ctxt

toList :: Context -> [(UntypedName,Scheme)]
toList (Gamma context) = M.toList context

-- |Instantiates scheme by applying a list of types.
-- Note that this is no instantiation in terms of Hindley-Milners type inferrence.
instantiateSchemeApp :: Scheme -> [Type] -> Scheme
instantiateSchemeApp = 
  let instantiate scheme t' = case scheme of
        SForall n s -> substitute (n,t') s
        SType t     -> case free t of
                        []    -> SType t
                        (f:_) -> SType $ substitute (untypedName f,t') t
  in
    foldl instantiate

-- |Unifies two types or fails if there is no substitution
unifyOrFail :: Monad m => Type -> Type -> m [Substitution]
unifyOrFail t1 t2 = case unify t1 t2 of
  Left msg -> fail msg
  Right s  -> return s

-- | Unifies a lists of types from left to right
unifyNorFail :: Monad m => [Type] -> m [Substitution]
unifyNorFail ts = case unifyN ts of
  Left msg -> fail msg
  Right s  -> return s

-- |@unify a b@ unifies the types @a@ and @b@. 
unify :: Type -> Type -> Either String [Substitution]
unify t1 t2 = case (t1,t2) of
  (TVar v, _)   -> return [(v, t2)]
  (_  , TVar v) -> return [(v, t1)]
   
  (TCon c1 ts1, TCon c2 ts2) -> 
     if c1 == c2 && (length ts1 == length ts2)
     then foldM (\s1 (t1,t2) -> do s2 <- unify (substituteN s1 t1) (substituteN s1 t2)
                                   return $ s1 ++ s2
                ) [] $ zip ts1 ts2
     else Left noUnifierFound
  where 
    noUnifierFound = "No unifier found for '" ++ show (pprint t1) ++ "' and '" ++ show (pprint t2) ++ "'"

-- | Unifies a lists of types from left to right
unifyN :: [Type] -> Either String [Substitution]
unifyN []  = Right []
unifyN [_] = Right []
unifyN (t:ts) = foldM (\(s,t1) t2 -> do
                            s' <- unify t1 (substituteN s t2) 
                            return (s ++ s', substituteN s' t1)
                      ) ([],t) ts
                      >>= return . fst
