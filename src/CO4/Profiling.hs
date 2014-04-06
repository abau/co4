{-# LANGUAGE LambdaCase #-}
module CO4.Profiling 
  ( Profile, emptyProfile, incNumCalls, incNumVariables, incNumClauses
  , onInnerUnder, onInner, onCurrentInner, currentFunction, setCurrentFunction
  , writeCurrentInner, incInnerUnderBy, printProfile
  )
where

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

emptyProfile :: Profile
emptyProfile = Profile M.empty M.empty "__init" emptyProfileData

onInnerUnder,onInner :: (ProfileMap -> ProfileMap) -> Profile -> Profile
onInnerUnder   f p = p { innerUnder   = f $ innerUnder   p }
onInner        f p = p { inner        = f $ inner        p }

onCurrentInner :: (ProfileData -> ProfileData) -> Profile -> Profile
onCurrentInner f p = p { currentInner = f $ currentInner p }

setCurrentFunction :: String -> Profile -> Profile
setCurrentFunction name p = p { currentFunction = name }

writeCurrentInner :: Profile -> Profile
writeCurrentInner p = 
  p { inner = M.alter (\case 
        Nothing -> Just i
        Just d  -> Just $ d { numCalls     = numCalls     d + numCalls     i
                            , numVariables = numVariables d + numVariables i
                            , numClauses   = numClauses   d + numClauses   i
                            }) (currentFunction p)
                               (inner           p)

    , currentInner = emptyProfileData
    }
  where
    i = currentInner p

incInnerUnderBy :: Int -> Int -> Int -> String -> Profile -> Profile
incInnerUnderBy calls vars clauses name p = 
  p { innerUnder = M.alter (\case
        Nothing -> Just $ ProfileData calls vars clauses 
        Just d  -> Just $ incAllBy calls vars clauses d
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
