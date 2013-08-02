{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module CO4.Compilation
  (compileFile, compile, stageNames)
where

import           Control.Monad.Reader 
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Q,addDependentFile)
import qualified Language.Haskell.Exts as HE
import           CO4.Language (Program)
import           CO4.Unique (MonadUnique,runUniqueT)
import           CO4.THUtil (unqualifiedNames,derive)
import           CO4.Util (addDeclarations)
import           CO4.Frontend
import           CO4.Frontend.HaskellSrcExts ()
import           CO4.Backend.TH (displayProgram)
import           CO4.Prelude (parsePrelude)
import           CO4.Config (MonadConfig,Config(..),configurable,is)
import qualified CO4.Config as C
import           CO4.Algorithms.Globalize (globalize)
import           CO4.Algorithms.UniqueNames (uniqueLocalNames)
import           CO4.Algorithms.Instantiation (instantiation)
import           CO4.Algorithms.EtaExpansion (etaExpansion)
import           CO4.Algorithms.SaturateApplication (saturateApplication)
import           CO4.Algorithms.Eitherize (eitherize)

stageParsed                 = "parsed"
stageUniqueLocalNames       = "uniqueLocalNames"
stageTypeInference          = "typeInference"
stageEtaExpansion           = "etaExpansion"
stageGlobalize              = "globalize"
stageSaturateApplication    = "saturateApplication"
stageInstantiation          = "instantiation"
stageSatchmo                = "satchmo"
stageSatchmoUnqualified     = "satchmoUnqualified"

stageNames                  = [ stageParsed
                              , stageUniqueLocalNames
                              , stageTypeInference
                              , stageEtaExpansion
                              , stageGlobalize
                              , stageSaturateApplication
                              , stageInstantiation
                              , stageSatchmo
                              , stageSatchmoUnqualified
                              ]

compileFile :: [Config] -> FilePath -> Q [TH.Dec]
compileFile configs filePath = 
  TH.runIO (HE.parseFile filePath) >>= \case
    HE.ParseOk _module -> do 
      addDependentFile filePath
      compile configs _module
    HE.ParseFailed loc msg -> error $ concat 
                                [ "Compilation.compileFile: can not compile `"
                                , filePath, "` (", msg, " at ", show loc, ")" ]

compile :: (ProgramFrontend a) => [Config] -> a -> Q [TH.Dec]
compile configs a = TH.runIO 
                  $ configurable configs 
                  $ runUniqueT 
                  $ do
  let instantiationDepth = C.instantiationDepth configs

  parsedPrelude <- is ImportPrelude >>= \case
                        True  -> parsePrelude
                        False -> return []

  parsedProgram <- parsePreprocessedProgram a 

  uniqueProgram <- lift ( dumpAfterStage' stageParsed 
                        $ addDeclarations parsedPrelude parsedProgram)

                >>= uniqueLocalNames 
                >>= lift . (dumpAfterStage' stageUniqueLocalNames)

                >>= etaExpansion
                >>= lift . (dumpAfterStage' stageEtaExpansion)

                >>= globalize 
                >>= lift . (dumpAfterStage' stageGlobalize)

                >>= saturateApplication
                >>= lift . (dumpAfterStage' stageSaturateApplication)

                >>= instantiation instantiationDepth
                >>= lift . (dumpAfterStage' stageInstantiation)

  result <- lift (is NoSatchmo) >>= \case
    True  -> return $ displayProgram parsedProgram
    False -> do satchmoP <- compileToSatchmo uniqueProgram
                return $ (derive ''Eq . derive ''Show) (displayProgram parsedProgram)
                      ++ satchmoP

  lift $ C.logWhenVerbose "Compilation successful"
  return result

dumpAfterStage' :: (MonadConfig m, MonadIO m) 
                => String -> Program -> m Program
dumpAfterStage' stage program = do
  C.dumpAfterStage stage $ displayProgram program
  return program

compileToSatchmo :: (MonadUnique m, MonadIO m, MonadConfig m) 
                 => Program -> m [TH.Dec]
compileToSatchmo program = do
  thProgram <- eitherize program 

  C.dumpAfterStage stageSatchmo $ show $ TH.ppr thProgram
  C.dumpAfterStage stageSatchmoUnqualified 
                 $ show $ TH.ppr 
                 $ unqualifiedNames thProgram
  return thProgram
