{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CO4.EncodedAdt.Overlapping
  (Overlapping (..),module CO4.EncodedAdt)
where

import           Prelude hiding (and,undefined)
import qualified Prelude as P
import           Control.Monad (forM)
import qualified Control.Exception as Exception 
import           Data.List (transpose)
import           Data.Maybe (catMaybes,fromJust)
import           Data.Tree (Tree (..),drawTree)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive 
  (Primitive,isConstant,evaluateConstant,constant,and)
import           Satchmo.Core.Decode (Decode,decode)
import           CO4.Util (for,toBinary,bitWidth,fromBinary)
import           CO4.EncodedAdt

data Overlapping p = Overlapping p [p] [ Overlapping p ] 
                   | Bottom
                     deriving (Eq,Ord)

instance (Primitive p,Show p) => Show (Overlapping p) where
  show = drawTree . toTree 
    where
      toTree adt | isConstantlyUndefined adt = Node "undefined" []
      toTree (Overlapping def fs conss)      = 
        Node (unwords [ "definedness:", show def, "flags:", show fs]) 
             (map toTree conss)
      toTree Bottom = Node "bottom" [] 

instance Primitive p => EncodedAdt Overlapping p where

  make fs = Overlapping (constant True) fs []

  undefined = Overlapping (constant False) [] []

  isBottom Bottom = True
  isBottom _      = False

  flags adt@(Overlapping _ fs _) = if isConstantlyUndefined adt
                                   then Nothing
                                   else Just fs
  flags Bottom = Nothing

  definedness (Overlapping d _ _) = d
  definedness Bottom              = constant True

  constantConstructorIndex adt = case flags adt of
    Nothing -> error "EncodedAdt.Overlapping.constantConstructorIndex: no flags"
    Just [] -> Just 0
    Just fs -> if all isConstant fs
               then Just $ fromBinary $ map (fromJust . evaluateConstant) fs
               else Nothing

  caseOf adt branches | isConstantlyUndefined adt 
                     || (all isConstantlyUndefined branches) 
 {- TODO: really? -} || (all (\x -> isConstantlyUndefined x || isBottom x) branches)
                      = return undefined
  caseOf adt branches | isBottom adt || (all isBottom branches)
                      = return Bottom
  caseOf adt branches | length (fromJust $ flags adt) < bitWidth (length branches) 
                      = return Bottom --error "EncodedAdt.Overlapping.caseOf: missing flags"
  caseOf adt branches =
    case constantConstructorIndex adt of
      Just i  -> Exception.assert (i < length branches) $ return $ branches !! i
      Nothing -> do 
        [branchDef] <- caseOfBits relevantFlags 
                     $ map (Just . return . definedness)        branches

        def'        <- and [branchDef, definedness adt]

        let adt'    = Overlapping def' relevantFlags (fromJust $ arguments adt)

        flags'      <- caseOfBits relevantFlags $ map flags     branches
        arguments'  <- caseOfArguments adt'     $ map arguments branches
        return $ Overlapping def' flags' arguments'
    where
      relevantFlags = take (bitWidth $ length branches) $ fromJust $ flags adt

  encodedConstructor i n = Exception.assert (i < n) 
                         . Overlapping (constant True) flags 
    where
      flags = case n of 1 -> []
                        _ -> map constant $ toBinary (Just $ bitWidth n) i

  constructorArgument _ _ adt | isConstantlyUndefined adt = undefined
  constructorArgument _ _ adt | isBottom adt              = Bottom
  constructorArgument i j adt@(Overlapping _ _ args) = 
    case constantConstructorIndex adt of
      Nothing           -> -- Exception.assert (i < length args) $ args !! i
                           if i < length args then args !! i
                                              else Bottom
      Just j' | j == j' -> Exception.assert (i < length args) $ args !! i
      Just _            -> Bottom

  toIntermediateAdt adt _ | isConstantlyUndefined adt = return IntermediateUndefined 
  toIntermediateAdt (Overlapping definedness flags args) n = 
    Exception.assert (length flags >= bitWidth n) $ do
      decode definedness >>= \case 
        False -> return IntermediateUndefined
        True  -> if null relevantFlags 
                 then return $ intermediate 0
                 else decode relevantFlags >>= return . intermediate . fromBinary
          where
            relevantFlags  = take (bitWidth n) flags
            intermediate i = IntermediateConstructorIndex i args 

arguments :: Primitive p => Overlapping p -> Maybe [ Overlapping p ]
arguments adt | isInvalid adt  = Nothing
arguments (Overlapping _ _ cs) = Just cs

caseOfArguments :: (MonadSAT m, Primitive p) => Overlapping p 
                                             -> [Maybe [Overlapping p] ]
                                             -> m [Overlapping p]
caseOfArguments adt branchArguments = 
  forM (transpose sameSizeBranchArguments) $ caseOf adt
  where 
    sameSizeBranchArguments = for branchArguments $ \case 
      Nothing   -> replicate maxArgs Bottom
      Just args -> args ++ replicate (maxArgs - length args) Bottom

    maxArgs = maximum $ map length $ catMaybes branchArguments
