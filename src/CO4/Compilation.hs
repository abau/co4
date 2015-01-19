{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CO4.Compilation
  (compileFile, compile)
where

import           Prelude hiding (log)
import           System.IO (stderr, hPutStrLn)
import           Control.Monad.Writer
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Q,addDependentFile)
import qualified Language.Haskell.Exts.Annotated as HE
import           CO4.Language (Program)
import           CO4.Unique (MonadUnique,UniqueT,withUniqueT,liftListen,liftPass)
import           CO4.THUtil (unqualifiedNames)
import           CO4.Util (addDeclarations)
import           CO4.Prelude (parsePrelude)
import           CO4.Config 
  (MonadConfig,Config(..),ConfigurableT,configurable,mapConfigurableT,is)
import qualified CO4.Config as C
import           CO4.Algorithms.Globalize (globalize)
import           CO4.Algorithms.UniqueNames (uniqueLocalNames)
import           CO4.Algorithms.HigherOrderInstantiation (hoInstantiation)
import           CO4.Algorithms.ExtendLambda (extendLambda)
import           CO4.Algorithms.SaturateApplication (saturateApplication)
import           CO4.Algorithms.THInstantiator (toTH)
import           CO4.CodeGen (codeGen,codeGenAdt)
import           CO4.PPrint (pprint)
import           CO4.Frontend.TH (parsePreprocessedTHDeclarations)
import           CO4.Frontend.HaskellSrcExts (toTHDeclarations)

type Message = String

compileFile :: [Config] -> FilePath -> Q [TH.Dec]
compileFile configs filePath = 
  TH.runIO (HE.parseFile filePath) >>= \case
    HE.ParseOk _module -> do 
      addDependentFile filePath
      compile (HideSource : configs) $ toTHDeclarations configs _module
    HE.ParseFailed loc msg -> error $ concat 
                                [ "Compilation.compileFile: can not compile `"
                                , filePath, "` (", msg, " at ", show loc, ")" ]

compile :: [Config] -> [TH.Dec] -> Q [TH.Dec]
compile configs program = do
  (program', msgs :: [Message]) <- runWriterT $ configurable configs 
                                              $ withUniqueT 
                                              $ compile' program
  case C.dumpTo configs of
    Nothing -> return ()
    Just "" -> TH.runIO $ hPutStrLn stderr $ unlines msgs
    Just fp -> TH.runIO $ writeFile fp     $ unlines msgs

  return program'

compile' :: (MonadWriter [Message] m, MonadUnique m, MonadConfig m) 
         => [TH.Dec] -> m [TH.Dec]
compile' program = do
  programWithPrelude <- do 
    parsedProgram <- parsePreprocessedTHDeclarations program

    is ImportPrelude >>= \case
      True  -> do parsedPrelude <- parsePrelude 
                  return $ addDeclarations parsedPrelude parsedProgram
      False -> return parsedProgram

  is OnlyAllocators >>= \case
    True  -> runCodeGenerator codeGenAdt programWithPrelude
    False -> do
      co4Program <-  uniqueLocalNames programWithPrelude
                 >>= extendLambda
                 >>= globalize 
                 >>= saturateApplication
                 >>= hoInstantiation

      dump "Concrete Program (CO4, after transformation)" $ show $ pprint co4Program

      result <- is NoSatchmo >>= \case
        True  -> return $ toTH co4Program
        False -> runCodeGenerator codeGen co4Program

      log "Compilation successful"

      is HideSource >>= \case
        False -> return $ program ++ result
        True  -> return result

runCodeGenerator :: (MonadUnique m , MonadWriter [Message] m, MonadConfig m) 
        => (Program -> m [TH.Dec]) -> Program -> m [TH.Dec]
runCodeGenerator generator program = do
  thProgram <- generator program 
  dump "Abstract Program (TH)" $ show $ TH.ppr $ unqualifiedNames thProgram
  return thProgram

dump :: (MonadWriter [Message] m, MonadConfig m) => String -> String -> m ()
dump header msg = tell [decoratedHeader, msg]
  where 
    decoratedHeader = concat ["\n## ", header, " ", replicate 30 '#', "\n"]

log :: MonadWriter [Message] m => String -> m ()
log x = tell [x]

instance MonadWriter [Message] m => MonadWriter [Message] (UniqueT m) where
  writer = lift . writer
  tell   = lift . tell
  listen = liftListen listen
  pass   = liftPass pass

instance MonadWriter [Message] m => MonadWriter [Message] (ConfigurableT m) where
  writer = lift . writer
  tell   = lift . tell
  listen = mapConfigurableT listen
  pass   = mapConfigurableT pass
