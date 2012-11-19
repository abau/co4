{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
module CO4.Algorithms.Eitherize.Util
where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Language.Haskell.TH as TH
import           CO4.Language 
import           CO4.Unique
import           CO4.THUtil
import           CO4.Util (isRecursiveAdt,countTConInConstructor)
import           CO4.Names (Namelike,mapName)

data Nat0
data NatSucc a

type Nat1 = NatSucc Nat0
type Nat2 = NatSucc Nat1
type Nat3 = NatSucc Nat2
type Nat4 = NatSucc Nat3
type Nat5 = NatSucc Nat4
type Nat6 = NatSucc Nat5

-- * 'SizedTypes'

-- |Mapping from type name to the number of size arguments of its sized equivalent
newtype SizedTypes = SizedTypes (M.Map UntypedName Int)

numSizeParameters :: UntypedName -> SizedTypes -> Int
numSizeParameters conName (SizedTypes st) = M.findWithDefault 
  (error $ "Algorithms.Eitherize.Util.numSizeParameters: " ++ show conName) 
  conName st

addToSizedTypes :: UntypedName -> Int -> SizedTypes -> SizedTypes
addToSizedTypes name i (SizedTypes st) = SizedTypes $ M.insert name i st

emptySizedTypes :: SizedTypes
emptySizedTypes = SizedTypes M.empty

countSizeParameters :: SizedTypes -> Declaration -> Int
countSizeParameters sizedTypes (DAdt adtName _ adtConss) = sum $ map inCons adtConss
  where inCons             = sum . map inType . cConArgumentTypes 
        inType (TVar _)    = 0
        inType (TCon c _)  | c == adtName = 0
        inType (TCon c ts) | otherwise    = 
         (numSizeParameters c sizedTypes) + (sum $ map inType ts)

-- * 'Gadt'

-- |Environment when handling sized types
data GadtEnv = GadtEnv { sizedTypes              :: SizedTypes
                       , sizeParameters          :: [UntypedName]
                       , mRecursiveSizeParameter :: Maybe UntypedName
                       }

-- |Stateful monad that counts the number of consumed size parameters when
-- traversing the constructors of an ADT
newtype Gadt u a = Gadt { fromGadt :: ReaderT GadtEnv (StateT Int u) a }
  deriving (Monad, Functor, MonadUnique, MonadReader GadtEnv, MonadState Int)

getSizeParameters :: Monad m => Gadt m [UntypedName]
getSizeParameters = asks sizeParameters

getRecursiveSizeParameter :: Monad m => Gadt m (Maybe UntypedName)
getRecursiveSizeParameter = asks mRecursiveSizeParameter

getAllSizeParameters :: Monad m => Gadt m [UntypedName]
getAllSizeParameters = do
  sizeParams    <- getSizeParameters
  mRecSizeParam <- getRecursiveSizeParameter
  case mRecSizeParam of
    Nothing -> return sizeParams
    Just p  -> return $ p : sizeParams

-- |Consumes the next @n@ size parameters, where @n@ depends on the passed constructor
-- name (see @SizedTypes@ mapping).
nextConstructorSizeParameters :: Monad m => UntypedName -> Gadt m [UntypedName]
nextConstructorSizeParameters conName = do
  n <- asks sizedTypes >>= return . numSizeParameters conName 
  forM [1..n] $ const $ nextSizeParameter

nextSizeParameter :: Monad m => Gadt m UntypedName
nextSizeParameter = do
  i <- get
  sizeArg <- asks $ \env -> sizeParameters env !! i
  modify (+1)
  return sizeArg

sizedName :: Namelike n => n -> n
sizedName = mapName ("Sized" ++)

resetSizeArgumentCounter :: Monad m => Gadt m ()
resetSizeArgumentCounter = put 0

-- |@runGadt s d a@ runs an action @a@ on an ADT @d@ inside the 'Gadt' monad.
-- All types, on which @d@ depends on, must appear in the 'SizedTypes' @s@.
runGadt :: MonadUnique u => SizedTypes -> Declaration -> Gadt u a -> u a
runGadt sizedTypes adt f = do
  let numSizeParams = countSizeParameters sizedTypes adt
  sizeParams    <- forM [1 .. numSizeParams] $ const $ newNamelike "argSize"
  mRecSizeParam <- if isRecursiveAdt adt then Just <$> newNamelike "recSize"
                                         else return Nothing

  let gadtEnv = GadtEnv sizedTypes sizeParams mRecSizeParam

  evalStateT ( runReaderT (fromGadt f) gadtEnv) 0

-- |Builds a list of constructor arguments of the GADT from an ADT.
-- @Left@ indicates recursive constructors.
-- @gadtConstructorArgs@ consumes all size parameters of 'Gadt'.
gadtConstructorArgs :: Monad m => Declaration -> Gadt m [Either [TH.Type] [TH.Type]]
gadtConstructorArgs (DAdt adtName _ adtConss) = mapM fromCons adtConss
  where
    fromCons cons = 
      if countTConInConstructor adtName cons > 0
      then Left  `liftM` mapM fromType (cConArgumentTypes cons)
      else Right `liftM` mapM fromType (cConArgumentTypes cons)

    fromType type_ = case type_ of
      TVar v -> return $ varT v

      TCon c ts | c == adtName -> do
        recParam   <- (varT . fromJust) `liftM` getRecursiveSizeParameter
        sizeParams <- map varT `liftM` getSizeParameters
        ts' <- mapM fromType ts
        return $ appsT (conT $ sizedName c) $ recParam : sizeParams ++ ts'

      TCon c ts | otherwise -> do
        sizeParams <- map varT `liftM` nextConstructorSizeParameters c
        ts' <- mapM fromType ts
        return $ appsT (conT $ sizedName c) $ sizeParams ++ ts'

-- * Various utilities

-- |@bindAndApply mapping f args@ binds @args@ to new names @ns@, maps $ns$ to 
-- expressions @es@ by @mapping@, applies @f@ to @es@ and
-- binds the result to a new name @r@. The last statement is @return r@.
bindAndApply :: MonadUnique u => ([Name] -> [TH.Exp]) -> ([TH.Exp] -> TH.Exp) 
                              -> [TH.Exp] -> u TH.Exp
bindAndApply mapping f args = do
  resultName <- newName "bindResult"
  argNames   <- forM args $ const $ newName "bindArgument"

  let bindings     = map (\(n,e) -> TH.BindS (varP n) e) $ zip argNames args
      applied      = f $ mapping argNames
      returnResult = [ TH.BindS     (varP resultName) applied
                     , TH.NoBindS $ returnE $ varE resultName
                     ]
  return $ TH.DoE $ bindings ++ returnResult

-- |@bindAndApplyArgs f args@ binds @args@ to new names @ns@,
-- applies @f@ to @ns@ and binds the result to a new name @r@. 
-- The last statement is @return r@.
bindAndApplyArgs :: MonadUnique u => ([TH.Exp] -> TH.Exp) 
                                  -> [TH.Exp] -> u TH.Exp
bindAndApplyArgs = bindAndApply (map varE) 

-- |@bindAndApplyList f args@ binds @args@ to new names @ns@,
-- applies @f@ to a list consisting of @ns@ and binds the result to a new name @r@. 
-- The last statement is @return r@.
bindAndApplyList :: MonadUnique u => (TH.Exp -> TH.Exp) 
                                  -> [TH.Exp] -> u TH.Exp
bindAndApplyList f = bindAndApply (\names -> [TH.ListE $ map varE names]) 
                                  (\[exp] -> f exp)
