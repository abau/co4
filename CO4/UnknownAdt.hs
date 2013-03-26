{-# language FlexibleContexts #-}
module CO4.UnknownAdt
  ( UnknownAdt (..), toTree, unknown, caseOf, encodedConsCall
  , constructorArgument, constructorArguments
  )
where

import           Prelude hiding (not,or,and)
import qualified Prelude as P
import           Control.Monad (liftM,forM,forM_,zipWithM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes,fromMaybe)
import           Data.Tree
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive 
  (Primitive,primitive,constant,assert,not,and,implies)
import           CO4.Util (replaceAt,equal,for,toBinary,binaries,bitWidth)
import qualified CO4.Allocator as A

--import Debug.Trace

data UnknownAdt p = UAdt { flags        :: [p]
                         , constructors :: [UnknownConstructor p]
                         }
                  | UUndefined

instance Show flag => Show (UnknownAdt flag) where
  show = drawTree . toTree 

toTree :: Show p => UnknownAdt p -> Tree String
toTree adt = case adt of
  UUndefined    -> Node "undefined" []
  UAdt fs conss -> Node ("flags: " ++ show fs) $ zipWith consToTree [0..] conss
  where
    consToTree i = \case UConstructor args -> Node ("cons " ++ show i) $ map toTree args
                         UBottom           -> Node ("cons " ++ show i ++ ": _|_") []

data UnknownConstructor p = UConstructor [UnknownAdt p] 
                          | UBottom
                          deriving (Show)

isDefined :: UnknownAdt p -> Bool
isDefined = \case UUndefined -> False
                  _          -> True

isBottom :: UnknownConstructor p -> Bool
isBottom = \case UBottom -> True
                 _       -> False

unknown :: (MonadSAT m, Primitive p) => A.AllocateUnknown -> m (UnknownAdt p)
unknown (A.AllocateUnknown cons) = do
  flags <- sequence $ replicate (bitWidth $ length cons) primitive
  cons  <- mapM encodeConstructor cons

  case cons of
    -- cf. single-constructor data types with indirect recursion:
    -- @data Rose a = Node a (List (Rose a))@
    [ UBottom ] -> return UUndefined 
    _ -> let uAdt = UAdt flags cons
         in do
           excludeBottom uAdt
           excludeInvalidConstructorPatterns uAdt
           return uAdt
  where
    encodeConstructor A.AllocateBottom             = return UBottom
    encodeConstructor (A.AllocateConstructor args) = do
      args' <- mapM unknown args
      if any (P.not . isDefined) args'
        then return   UBottom
        else return $ UConstructor args'

excludeBottom :: (MonadSAT m, Primitive p) => UnknownAdt p -> m ()
excludeBottom = go 
  where
    go adt = forM_ (zip [0..] (constructors adt)) $ uncurry 
                                                  $ goConstructor 
                                                  $ flags adt

    goConstructor _ _     (UConstructor args) = forM_ args go 
    goConstructor flags i  UBottom            = 
      let pattern = toBinary (length flags) i
      in
        excludePattern flags pattern

excludeInvalidConstructorPatterns :: (MonadSAT m, Primitive p) => UnknownAdt p -> m ()
excludeInvalidConstructorPatterns = go
  where
    go adt = do
      forM_ nonConstructorPatterns $ excludePattern $ flags adt
      forM_ (constructors adt) goConstructor 

      where
        nonConstructorPatterns = drop (length $ constructors adt)
                               $ binaries $ length $ flags adt

    goConstructor (UConstructor args) = forM_ args go
    goConstructor  UBottom            = return ()

caseOf :: (MonadSAT m, Primitive p) => UnknownAdt p -> [UnknownAdt p] -> m (UnknownAdt p)
caseOf adt branches = 
  if allBranchesUndefined -- !
  then return UUndefined
  else do
    flags'        <- caseOfBits (flags adt) $ mapBranches flags
    constructors' <- caseOfConstructors adt $ mapBranches constructors
    return $ UAdt flags' constructors'
  where
    allBranchesUndefined = all (P.not . isDefined) branches

    mapBranches f = for branches $ \case UUndefined -> Nothing
                                         branch     -> Just $ f branch

caseOfConstructors :: (MonadSAT m, Primitive p) => UnknownAdt p 
                                                -> [Maybe [UnknownConstructor p]] 
                                                -> m [UnknownConstructor p]
caseOfConstructors adt conss = 
  forM (transpose conss') $ \consT -> 
    if all isBottom consT 
    then return UBottom
    else liftM UConstructor 
       $ mapM (caseOf adt) 
       $ transpose 
       $ map (getArgs $ numConsArgs consT) consT
  where 
    conss' = for conss $ \case Just cons -> Exception.assert (length cons == numCons) 
                                            cons
                               Nothing   -> replicate numCons UBottom

    numCons = length $ head $ catMaybes conss

    numConsArgs cons = length args
      where UConstructor args = head $ filter (P.not . isBottom) cons

    getArgs n  UBottom            = replicate n UUndefined
    getArgs n (UConstructor args) = Exception.assert (n == length args) args

caseOfBits :: (MonadSAT m, Primitive p) => [p] -> [Maybe [p]] -> m [p]
caseOfBits flags bitss = Exception.assert (P.not $ null definedBitss ) $
                         Exception.assert (equal length definedBitss) $ do
  premises <- mkPremises
  forM (transpose bitss') $ mkBits premises
  where
    definedBitss = catMaybes bitss
    bitWidth     = length $ head definedBitss
    bitss'       = map (fromMaybe $ replicate bitWidth $ constant False) bitss
    mkPremises   = mapM mkPremise patterns 
      where 
        patterns                = binaries $ length flags 
        selectFlag True  flag   = flag
        selectFlag False flag   = not flag

        mkPremise pattern = and $ zipWith selectFlag pattern flags

    mkBits premises bitsT = zipWithM implies premises bitsT >>= and

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelectBit flags pattern
  where 
    antiSelectBit bit True  = not bit
    antiSelectBit bit False = bit

encodedConsCall :: Primitive p => Int -> Int -> [UnknownAdt p] -> UnknownAdt p
encodedConsCall i numCons args = Exception.assert (i < numCons) $ 
  if containsUndefinedArgs
  then UUndefined
  else UAdt flags' constructors'
  where
    flags'                = if numCons > 1 then map constant i' else []
    constructors'         = replaceAt i (UConstructor args) 
                          $ replicate numCons UBottom
    i'                    = toBinary (bitWidth numCons) i 
    containsUndefinedArgs = any (P.not . isDefined) args

constructorArgument :: Int -> Int -> UnknownAdt p -> UnknownAdt p
constructorArgument i j = maybe UUndefined getArg . constructorArguments j
  where 
    getArg args = Exception.assert (i < length args) $ args !! i

constructorArguments :: Int -> UnknownAdt p -> Maybe [UnknownAdt p]
constructorArguments _  UUndefined = Nothing
constructorArguments j (UAdt _ cs) = Exception.assert (j < length cs) $
  case cs !! j of
    UConstructor as -> Just as
    UBottom         -> Nothing
