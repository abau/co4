{-# LANGUAGE LambdaCase #-}
module CO4.Allocator 
  ( Allocator (..), AllocateConstructor (..)
  , known, CO4.Allocator.constructors, bottom, allocates
  )
where

import qualified Control.Exception as Exception
import           Control.Monad (liftM,forM_)
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive (Primitive,primitive,antiSelect,assert)
import qualified Satchmo.Core.Boolean
import           CO4.Encodeable (Encodeable (..))
import           CO4.EncodedAdt (EncodedAdt (..), UnknownConstructor (..),isDefined)
import           CO4.Util (bitWidth,binaries,toBinary)

data Allocator = Known { constructorIndex :: Int
                       , numConstructors  :: Int
                       , arguments        :: [Allocator]
                       }
               | Unknown [AllocateConstructor]

data AllocateConstructor = AllocateConstructor [Allocator]
                         | AllocateBottom

known :: Int -> Int -> [Allocator] -> Allocator
known = Known

constructors :: [Maybe [Allocator]] -> Allocator
constructors allocs = Exception.assert (not $ null allocs) 
                    $ Unknown $ map toConstructor allocs
  where
    toConstructor Nothing     = AllocateBottom
    toConstructor (Just args) = AllocateConstructor args

bottom :: AllocateConstructor
bottom = AllocateBottom

instance Encodeable Allocator where
  encode = \case
    Known i n as -> liftM (KConstructor i n) $ mapM encode as
    Unknown [allocCons] -> encodeConstructor allocCons >>= \case
      UBottom           -> return Undefined
      UConstructor args -> return $ KConstructor 0 1 args

    Unknown allocConss -> do
      flags <- sequence $ replicate (bitWidth $ length allocConss) primitive
      cons  <- mapM encodeConstructor allocConss
      let uAdt = UAdt flags cons
      excludeBottom uAdt
      excludeInvalidConstructorPatterns uAdt
      return uAdt
    where
      encodeConstructor AllocateBottom             = return UBottom
      encodeConstructor (AllocateConstructor args) = do
        args' <- mapM encode args
        if any (not . isDefined) args'
          then return   UBottom
          else return $ UConstructor args'

excludeBottom :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeBottom = go 
  where
    go (KConstructor _ _ args) = forM_ args go
    go (UAdt flags conss)      = forM_ (zip [0..] conss) $ uncurry 
                                                         $ goConstructor flags 
    goConstructor _ _     (UConstructor args) = forM_ args go 
    goConstructor flags i  UBottom            = 
      let pattern = toBinary (length flags) i
      in
        excludePattern flags pattern

excludeInvalidConstructorPatterns :: (MonadSAT m, Primitive p) => EncodedAdt p -> m ()
excludeInvalidConstructorPatterns = go
  where
    go (KConstructor _ _ args) = forM_ args go
    go (UAdt flags conss)      = do
      forM_ nonConstructorPatterns $ excludePattern flags 
      forM_ conss goConstructor 

      where
        nonConstructorPatterns = drop (length conss) $ binaries $ length flags 

    goConstructor (UConstructor args) = forM_ args go
    goConstructor  UBottom            = return ()

excludePattern :: (MonadSAT m, Primitive p) => [p] -> [Bool] -> m ()
excludePattern flags pattern = Exception.assert (length flags == length pattern)
                             $ assert $ zipWith antiSelect pattern flags 

allocates :: Monad m => Allocator -> m (EncodedAdt Satchmo.Core.Boolean.Boolean)
                     -> m ( Maybe ([(Int,Int)],String) )
allocates _allocator _adt = _adt >>= return . go [] _allocator
  where 
    go path allocator adt = case (allocator,adt) of
      (Known i n as, KConstructor i' n' as') 
        | and [ i == i', n == n', length as == length as'] ->
          if null as 
          then Nothing 
          else 
            foldl (\result (j,a,a') -> case result of
                      Just r  -> Just r
                      Nothing -> go (path ++ [(i,j)]) a a'
                  ) Nothing $ zip3 [0..] as as'

      (Known i _ _, KConstructor i' _ _) | i /= i' ->
        Just (path, unwords [ "Allocator.Known.constructorIndex:", show i, "/="
                            , "EncodedAdt.KConstructor.constructorIndex:", show i' ])

      (Known _ n _, KConstructor _ n' _) | n /= n' ->
        Just (path, unwords [ "Allocator.Known.numConstructors:", show n, "/="
                            , "EncodedAdt.KConstructor.numConstructors:", show n' ])

      (Known _ _ as, KConstructor _ _ as') | length as /= length as' ->
        Just (path, unwords [ "Allocator.Known.|arguments|:", show (length as), "/="
                            , "EncodedAdt.KConstructor.|arguments|:", show (length as') ])

      (Known i n as, UAdt _ conss) ->
        if i < length conss && n == length conss
        then goConstructor path i (AllocateConstructor as) (conss !! i)
        else Just (path, unwords [ "Allocator.Known", show i, show n
                                 , "... /= EncodedAdt.UAdt.|constructors|" ])

      (Unknown conss, UAdt _ conss') | length conss == length conss' ->
          if null conss
          then Nothing 
          else 
            foldl (\result (i,c,c') -> case result of
                      Just r  -> Just r
                      Nothing -> goConstructor path i c c'
                  ) Nothing $ zip3 [0..] conss conss'

      (Unknown conss, KConstructor i n as) ->
        if i < length conss && n == length conss
        then goConstructor path i (conss !! i) (UConstructor as)
        else Just (path, unwords [ "Allocator.Unknown.|constructors| /=" 
                                 , "EncodedAdt.KConstructor", show i, show n, "..." ])

    goConstructor path i allocator constructor = case (allocator, constructor) of
      (AllocateBottom, UBottom) -> Nothing
      (AllocateBottom, UConstructor _) -> 
        Just (path, "Allocator.AllocateBottom /= EncodedAdt.UConstructor")
      (AllocateConstructor {}, UBottom) ->
        Nothing
      (AllocateConstructor as, UConstructor as') | length as == length as' ->
        foldl (\result (j,a,a') -> case result of
                  Just r  -> Just r
                  Nothing -> go (path ++ [(i,j)]) a a'
              ) Nothing $ zip3 [0..] as as'
      (AllocateConstructor as, UConstructor as') | length as /= length as' ->
        Just (path, "Allocator.AllocateConstructor.|arguments| /= EncodedAdt.UConstructor.|arguments|")
