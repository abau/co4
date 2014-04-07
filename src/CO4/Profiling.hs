{-# LANGUAGE LambdaCase #-}
module CO4.Profiling 
  ( ProfileData, numCalls, numVariables, numClauses, incNumVariables, incNumClauses
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

data ProfileData = ProfileData { numCalls     :: ! Int
                               , numVariables :: ! Int
                               , numClauses   :: ! Int
                               } 
                               deriving Show

emptyProfileData :: ProfileData
emptyProfileData = ProfileData 0 0 0

isEmptyProfileData :: ProfileData -> Bool
isEmptyProfileData d = and [ numCalls d == 0, numVariables d == 0, numClauses d == 0 ]

incNumCalls,incNumVariables,incNumClauses :: ProfileData -> ProfileData
incNumCalls     c = c { numCalls     = 1 + numCalls     c }
incNumVariables c = c { numVariables = 1 + numVariables c }
incNumClauses   c = c { numClauses   = 1 + numClauses   c }

incAllBy :: Int -> Int -> Int -> ProfileData -> ProfileData
incAllBy calls vars clauses p = 
  p { numCalls     = calls   + numCalls     p
    , numVariables = vars    + numVariables p
    , numClauses   = clauses + numClauses   p
    }

type ProfileMap = M.Map String ProfileData

data Profile = Profile { innerUnder      :: ! ProfileMap
                       , inner           :: ! ProfileMap
                       , currentFunction :: ! String
                       , currentInner    :: ! ProfileData
                       }

initName :: String
initName = "__init"

emptyProfile :: Profile
emptyProfile = Profile M.empty M.empty initName emptyProfileData

onCurrentInner :: (ProfileData -> ProfileData) -> Profile -> Profile
onCurrentInner f p = p { currentInner = f $ currentInner p }

resetCurrentInner :: Profile -> Profile
resetCurrentInner p = p { currentInner = emptyProfileData }

callFunction :: String -> Profile -> Profile
callFunction name p = assert (isEmptyProfileData $ currentInner p) $
  p { currentFunction = name 
    , innerUnder      = M.alter (\case 
                          Nothing -> Just $ emptyProfileData { numCalls = 1 }
                          Just d  -> Just $ incNumCalls d
                        ) name $ innerUnder p
    , currentInner    = emptyProfileData { numCalls = 1 }
    }

returnTo :: String -> ProfileData -> Profile -> Profile
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

printProfile :: (MonadIO m) => Profile -> m ()
printProfile profile = do 
  printProfileMap "inner-under" innerUnder profile
  printProfileMap "inner"       inner      profile
  where
    printProfileMap msg p profile =
      when (not $ M.null $ p profile) $
        liftIO $ hPutStrLn stderr $ unlines 
               $ (concat ["Profiling (",msg,"): "] :) 
               $ map show 
               $ reverse
               $ sortBy (compare `on` (numVariables . snd))
               $ M.toList $ p profile
