{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Compilation
  (compileFile, compile)
where

import           Prelude hiding (log)
import           System.IO (stderr, hPutStrLn)
import           Control.Monad.Reader 
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Q,addDependentFile)
import qualified Language.Haskell.Exts as HE
import           CO4.Language (Program)
import           CO4.Unique (MonadUnique,runUniqueT)
import           CO4.THUtil (unqualifiedNames)
import           CO4.Util (addDeclarations)
import           CO4.Prelude (parsePrelude)
import           CO4.Config (MonadConfig,Config(..),configurable,is,fromConfigs)
import qualified CO4.Config as C
import           CO4.Algorithms.Globalize (globalize)
import           CO4.Algorithms.UniqueNames (uniqueLocalNames)
import           CO4.Algorithms.HigherOrderInstantiation (hoInstantiation)
import           CO4.Algorithms.ExtendLambda (extendLambda)
import           CO4.Algorithms.SaturateApplication (saturateApplication)
import           CO4.Algorithms.PolymorphicInstantiation (polyInstantiation)
import           CO4.Algorithms.THInstantiator (toTH)
import           CO4.CodeGen (codeGen)
import           CO4.PPrint (pprint)
import           CO4.Frontend.TH (parsePreprocessedTHDeclarations)
import           CO4.Frontend.HaskellSrcExts (toTHDeclarations)

compileFile :: [Config] -> FilePath -> Q [TH.Dec]
compileFile configs filePath = 
  TH.runIO (HE.parseFile filePath) >>= \case
    HE.ParseOk _module -> do 
      addDependentFile filePath
      compile (HideSource : configs) $ toTHDeclarations _module
    HE.ParseFailed loc msg -> error $ concat 
                                [ "Compilation.compileFile: can not compile `"
                                , filePath, "` (", msg, " at ", show loc, ")" ]

compile :: [Config] -> [TH.Dec] -> Q [TH.Dec]
compile configs program = TH.runIO 
                        $ configurable configs 
                        $ runUniqueT 
                        $ do

  parsedPrelude <- is ImportPrelude >>= \case
                        True  -> parsePrelude
                        False -> return []

  co4Program <-  parsePreprocessedTHDeclarations program
             >>= return . addDeclarations parsedPrelude
             >>= uniqueLocalNames 
             >>= extendLambda
             >>= globalize 
             >>= saturateApplication
             >>= hoInstantiation
             >>= polyInstantiation

  result <- is NoSatchmo >>= \case
    True  -> do dump $ show $ pprint co4Program
                return $ toTH co4Program

    False -> do satchmoProgram <- compileToSatchmo co4Program
                return satchmoProgram

  liftIO $ log "Compilation successful"

  is HideSource >>= \case
    False -> return $ program ++ result
    True  -> return result

compileToSatchmo :: (MonadUnique m, MonadIO m, MonadConfig m) 
                 => Program -> m [TH.Dec]
compileToSatchmo program = do
  thProgram <- codeGen program 
  dump $ show $ TH.ppr $ unqualifiedNames thProgram
  return thProgram

dump :: (MonadIO m, MonadConfig m) => String -> m ()
dump msg = fromConfigs C.dumpTo >>= \case
  Nothing -> return ()
  Just "" -> log msg
  Just f  -> liftIO $ writeFile f msg

log :: MonadIO m => String -> m ()
log = liftIO . hPutStrLn stderr 
