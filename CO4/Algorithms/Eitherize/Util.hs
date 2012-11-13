{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
module CO4.Algorithms.Eitherize.Util
where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map as M
import qualified Language.Haskell.TH as TH
import           CO4.Language 
import           CO4.Unique
import           CO4.THUtil
import           CO4.Util (isRecursiveAdt)
import           CO4.Names (Namelike,mapName)

data Nat0
data NatSucc a

type Nat1 = NatSucc Nat0
type Nat2 = NatSucc Nat1
type Nat3 = NatSucc Nat2
type Nat4 = NatSucc Nat3
type Nat5 = NatSucc Nat4
type Nat6 = NatSucc Nat5

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

-- |Environment when handling sized types
data GadtEnv = GadtEnv { sizedTypes              :: SizedTypes
                       , sizeParameters          :: [UntypedName]
                       , mRecursiveSizeParameter :: Maybe UntypedName
                       }

-- |Stateful monad that counts the number of consumed size parameters when
-- traversing the constructors of ADT
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

-- |Consumes the next @n@ size parameters, where @n@ depends on the constructor's
-- name, that is currently traversed (see @SizedTypes@ mapping).
nextConstructorSizeParameters :: Monad m => UntypedName -> Gadt m [UntypedName]
nextConstructorSizeParameters conName = do
  n <- asks sizedTypes >>= return . numSizeParameters conName 
  forM [1..n] $ const $ nextSizeArgument

nextSizeArgument :: Monad m => Gadt m UntypedName
nextSizeArgument = do
  i <- get
  sizeArg <- asks $ \env -> sizeParameters env !! i
  modify (+1)
  return sizeArg

sizedName :: Namelike n => n -> n
sizedName = mapName ("Sized" ++)

resetSizeArgumentCounter :: Monad m => Gadt m ()
resetSizeArgumentCounter = put 0

runGadt :: MonadUnique u => SizedTypes -> Declaration -> Gadt u a -> u a
runGadt sizedTypes adt f = do
  let numSizeParams = countSizeParameters sizedTypes adt
  sizeParams    <- forM [1 .. numSizeParams] $ const $ newNamelike "argSize"
  mRecSizeParam <- if isRecursiveAdt adt then Just <$> newNamelike "recSize"
                                         else return Nothing

  let gadtEnv = GadtEnv sizedTypes sizeParams mRecSizeParam

  evalStateT ( runReaderT (fromGadt f) gadtEnv) 0


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
