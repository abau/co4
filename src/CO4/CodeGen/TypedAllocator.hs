{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module CO4.CodeGen.TypedAllocator
  (allocators)
where

import           Control.Monad (forM)
import qualified Language.Haskell.TH as TH
import           CO4.Language
import           CO4.Unique
import           CO4.Names (Namelike,mapName,toValidDataIdentifier)
import           CO4.Allocator.Data
import           CO4.Allocator.Typed
import           CO4.THUtil
import           CO4.Algorithms.THInstantiator (toTH)
import           CO4.TypesUtil (typeOfAdt,functionType)
import           CO4.Util (tCon)

allocators :: MonadUnique u => Adt -> u [TH.Dec]
allocators adt = do
  kCons  <- knownConsAllocators adt
  fromKs <- fromKnownAllocator  adt
  return $ concat [kCons, fromKs, completeAllocator adt ]

knownConsAllocators :: MonadUnique u => Adt -> u [TH.Dec]
knownConsAllocators adt =
  sequence (zipWith (knownConsAllocator adt) [0..] cons) >>= return . concat
  where
    cons = adtConstructors adt
    
knownConsAllocatorName :: (Eq n, Namelike n) => n -> n
knownConsAllocatorName = mapName ("known" ++) . toValidDataIdentifier

knownConsAllocator :: MonadUnique u => Adt -> Int -> Constructor -> u [TH.Dec]
knownConsAllocator adt i (CCon cName cArgTypes) = 
  sequence [ mkSignature, mkDefinition ]
  where
    allocName    = knownConsAllocatorName cName
    allocType    = typeOfAdt adt
    numCons      = length $ adtConstructors adt

    mkSignature  = return 
                 $ sigD' allocName 
                 $ toTH
                 $ foldr SForall
                   ( SType $ functionType (map tAllocatorType cArgTypes) (tAllocatorType allocType) )
                   ( adtTypeVariables adt )

    mkDefinition = do
      ns <- forM cArgTypes $ const $ newName ""
      return $ funD' allocName (patterns ns) (body ns)
      where
        patterns  = map varP
        body ns   = TH.AppE (TH.VarE 'unsafeTAllocator)
                  $ appsE (TH.VarE 'known) [intE i, intE numCons, TH.ListE $ map toAlloc ns]
        toAlloc n = TH.AppE (TH.VarE 'toAllocator) $ varE n

fromKnownAllocator :: MonadUnique u => Adt -> u [TH.Dec]
fromKnownAllocator adt = do
  def <- mkDefinition
  return [ TH.InstanceD predicates instType [def] ]
  where
    allocType    = typeOfAdt adt
    typeVars     = adtTypeVariables adt

    predicates   = map (\v -> TH.ClassP ''FromKnown [varT v]) typeVars
    instType     = TH.AppT (TH.ConT ''FromKnown) $ toTH allocType

    mkDefinition = do 
      arg     <- newName ""
      matches <- forM (adtConstructors adt) mkMatch
      return $ funD' "fromKnown" [varP arg] 
             $ caseE (varE arg) matches

    mkMatch (CCon cName cTypes) = do
      names <- forM cTypes $ const $ newName ""
      return $ (pattern names, branch names)
      where
        pattern = conP cName . map varP
        branch  = appsE (varE $ knownConsAllocatorName cName)
                . map toArg 

        toArg name = TH.AppE (TH.VarE 'fromKnown) $ varE name

completeAllocator :: Adt -> [TH.Dec]
completeAllocator adt = [ TH.InstanceD predicates instType [def] ]
  where
    allocType  = typeOfAdt adt
    typeVars   = adtTypeVariables adt

    predicates = map (\v -> TH.ClassP ''Complete [varT v]) typeVars
    instType   = TH.AppT (TH.ConT ''Complete) $ toTH allocType

    def        = funD' "complete" [] body
      where
        body = TH.AppE (TH.VarE 'unions)
             $ TH.ListE 
             $ map toConAlloc
             $ adtConstructors adt

        toConAlloc (CCon c cArgs) = appsE (varE $ knownConsAllocatorName c)
                                  $ map (const $ varE "complete") cArgs

tAllocatorType :: Type -> Type
tAllocatorType t = tCon "TAllocator" [t]
