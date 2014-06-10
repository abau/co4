{-# LANGUAGE LambdaCase #-}
module CO4.Profiling 
  ( numCalls, numVariables, numClauses, incNumVariables, incNumClauses
  , profileCase
  , Profile, currentFunction, currentInner, emptyProfile, onCurrentInner
  , resetCurrentInner, callFunction, returnTo, incInnerUnderBy, printProfile
  )
where

import           Control.Exception (assert)
import           Control.Monad.IO.Class
import           Control.Monad (when)
import           Data.List (sortBy)
import           Data.Function (on)
import qualified Data.Map.Strict as M

import System.IO ( stderr, hPutStrLn)

data FunProfileData = FunProfileData { numCalls     :: ! Int
                                     , numVariables :: ! Int
                                     , numClauses   :: ! Int
                                     } 
                                     deriving Show

emptyFunProfileData :: FunProfileData
emptyFunProfileData = FunProfileData 0 0 0

isEmptyFunProfileData :: FunProfileData -> Bool
isEmptyFunProfileData d = and [ numCalls d == 0, numVariables d == 0, numClauses d == 0 ]

incNumCalls,incNumVariables,incNumClauses :: FunProfileData -> FunProfileData
incNumCalls     c = c { numCalls     = 1 + numCalls     c }
incNumVariables c = c { numVariables = 1 + numVariables c }
incNumClauses   c = c { numClauses   = 1 + numClauses   c }

incAllBy :: Int -> Int -> Int -> FunProfileData -> FunProfileData
incAllBy calls vars clauses p = 
  p { numCalls     = calls   + numCalls     p
    , numVariables = vars    + numVariables p
    , numClauses   = clauses + numClauses   p
    }

data CaseProfileData = CaseProfileData { numEvaluations :: ! Int
                                       , numKnown       :: ! Int
                                       , numUnknown     :: ! Int
                                       }
                                       deriving Show

emptyCaseProfileData :: CaseProfileData
emptyCaseProfileData = CaseProfileData 0 0 0

incNumKnown :: CaseProfileData -> CaseProfileData
incNumKnown c = c { numEvaluations = 1 + numEvaluations c
                  , numKnown       = 1 + numKnown       c
                  }

incNumUnknown :: CaseProfileData -> CaseProfileData
incNumUnknown c = c { numEvaluations = 1 + numEvaluations c
                    , numUnknown     = 1 + numUnknown     c
                    }

type FunProfileMap  = M.Map String    FunProfileData
type CaseProfileMap = M.Map (Int,Int) CaseProfileData

data Profile = Profile { innerUnder      :: ! FunProfileMap
                       , inner           :: ! FunProfileMap
                       , currentFunction :: ! String
                       , currentInner    :: ! FunProfileData
                       , caseProfile     :: ! CaseProfileMap
                       }

initName :: String
initName = "__init"

emptyProfile :: Profile
emptyProfile = Profile M.empty M.empty initName emptyFunProfileData M.empty

onCurrentInner :: (FunProfileData -> FunProfileData) -> Profile -> Profile
onCurrentInner f p = p { currentInner = f $ currentInner p }

resetCurrentInner :: Profile -> Profile
resetCurrentInner p = p { currentInner = emptyFunProfileData }

callFunction :: String -> Profile -> Profile
callFunction name p = assert (isEmptyFunProfileData $ currentInner p) $
  p { currentFunction = name 
    , innerUnder      = M.alter (\case 
                          Nothing -> Just $ emptyFunProfileData { numCalls = 1 }
                          Just d  -> Just $ incNumCalls d
                        ) name $ innerUnder p
    , currentInner    = emptyFunProfileData { numCalls = 1 }
    }

returnTo :: String -> FunProfileData -> Profile -> Profile
returnTo previousName previousInner = returnTo' . writeCurrentInner
  where
    returnTo' p = 
      p { currentFunction = previousName
        , currentInner    = previousInner
        }

    writeCurrentInner p = assert (numCalls i == 1) $
      p { inner = M.alter (\case 
            Nothing -> Just i
            Just d  -> Just $ d { numCalls     = numCalls     d + numCalls     i
                                , numVariables = numVariables d + numVariables i
                                , numClauses   = numClauses   d + numClauses   i
                                }) (currentFunction p)
                                   (inner           p)
        }
      where
        i = currentInner p

incInnerUnderBy :: Int -> Int -> String -> Profile -> Profile
incInnerUnderBy vars clauses name p = 
  p { innerUnder = M.alter (\case
          Nothing | name == initName -> Nothing
          Nothing -> error $ "Profiling.incInnerUnderBy: '" ++ name ++ "'"
          Just d  -> Just $ incAllBy 0 vars clauses d
        ) name $ innerUnder p 
    }

profileCase :: (Int, Int) -> Bool -> Profile -> Profile
profileCase pos isKnown p = 
  p { caseProfile = M.alter (\case
        Nothing -> Just $ inc $ emptyCaseProfileData
        Just d  -> Just $ inc d
      ) pos $ caseProfile p
    }
  where
    inc = if isKnown then incNumKnown else incNumUnknown

printProfile :: (MonadIO m) => Profile -> m ()
printProfile profile = do 
  printFunProfileMap  "inner-under" $ innerUnder  profile
  printFunProfileMap  "inner"       $ inner       profile
  printCaseProfileMap               $ caseProfile profile
  where
    printFunProfileMap msg m =
      when (not $ M.null m) $
        liftIO $ hPutStrLn stderr $ unlines 
               $ (concat ["Profiling (",msg,"): "] :) 
               $ map show 
               $ reverse
               $ sortBy (compare `on` (numVariables . snd))
               $ M.toList m

    printCaseProfileMap m =
      when (not $ M.null m) $
        liftIO $ hPutStrLn stderr $ unlines 
               $ ("Cases: " :) 
               $ map show 
               $ reverse
               $ sortBy (compare `on` (numUnknown . snd))
               $ M.toList m
