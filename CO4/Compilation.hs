{-# LANGUAGE ScopedTypeVariables #-}
{-# language LambdaCase #-}

module CO4.Compilation
  (compileFile, compile, stageNames)
where

import           Control.Monad.Reader 
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Quasi)
import qualified Language.Haskell.Exts as HE
import           CO4.Language (Program)
import           CO4.Unique (MonadUnique,runUniqueT)
import           CO4.THUtil (unqualifiedNames,deriveShows)
import           CO4.Util (addDeclarations)
import           CO4.Frontend
import           CO4.Frontend.HaskellSrcExts ()
import           CO4.Backend.TH (displayProgram)
import           CO4.Prelude (parsePrelude)
import           CO4.Config (MonadConfigurable,Config(..),is)
import qualified CO4.Config as C
import           CO4.Algorithms.Globalize (globalize)
import           CO4.Algorithms.UniqueNames (uniqueLocalNames)
import           CO4.Algorithms.Instantiation (instantiation)
import           CO4.Algorithms.EtaExpansion (etaExpansion)
import           CO4.Algorithms.SaturateApplication (saturateApplication)
import           CO4.Algorithms.Eitherize (eitherize)
import           CO4.Algorithms.HindleyMilner (schemes)
import           CO4.Algorithms.Util (eraseTypedNames)

stageParsed                 = "parsed"
stageUniqueLocalNames       = "uniqueLocalNames"
stageTypeInference          = "typeInference"
stageEtaExpansion           = "etaExpansion"
stageCloseLocalAbstractions = "closeLocalAbstractions"
stageGlobalize              = "globalize"
stageSaturateApplication    = "saturateApplication"
stageInstantiation          = "instantiation"
stageSatchmo                = "satchmo"
stageSatchmoUnqualified     = "satchmoUnqualified"

stageNames                  = [ stageParsed
                              , stageUniqueLocalNames
                              , stageTypeInference
                              , stageEtaExpansion
                              , stageCloseLocalAbstractions
                              , stageGlobalize
                              , stageSaturateApplication
                              , stageInstantiation
                              , stageSatchmo
                              , stageSatchmoUnqualified
                              ]

compileFile :: (MonadConfigurable m, MonadIO m, Quasi m) => FilePath -> m [TH.Dec]
compileFile filePath = 
  liftIO (HE.parseFile filePath) >>= \case
    HE.ParseOk _module     -> compile _module
    HE.ParseFailed loc msg -> error $ concat 
                                [ "Compilation.compileFile: can not compile `"
                                , filePath, "` (", msg, " at ", show loc, ")" ]

compile :: (ProgramFrontend a, MonadConfigurable m, MonadIO m, Quasi m) 
        => a -> m [TH.Dec]
compile a = do
  instantiationDepth <- C.fromConfigs C.instantiationDepth

  runUniqueT $ do
    parsedPrelude <- is ImportPrelude >>= \case
                          True  -> parsePrelude
                          False -> return []

    parsedProgram <- parsePreprocessedProgram a 

    uniqueProgram <- lift ( dumpAfterStage' stageParsed 
                          $ addDeclarations parsedPrelude parsedProgram)

                  >>= uniqueLocalNames 
                  >>= lift . (dumpAfterStage' stageUniqueLocalNames)

                  >>= schemes
                  >>= lift . (dumpAfterStage'   stageTypeInference)
                  >>= return . eraseTypedNames

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
                            return $ deriveShows (displayProgram parsedProgram)
                                  ++ satchmoP

    lift $ C.logWhenVerbose "Compilation successful"
    return result

dumpAfterStage' :: (MonadConfigurable m, MonadIO m) 
                => String -> Program -> m Program
dumpAfterStage' stage program = do
  C.dumpAfterStage stage $ displayProgram program
  return program

compileToSatchmo :: (MonadUnique m, MonadIO m, MonadConfigurable m) 
                 => Program -> m [TH.Dec]
compileToSatchmo program = do
  thProgram <- eitherize program

  C.dumpAfterStage stageSatchmo $ show $ TH.ppr thProgram
  C.dumpAfterStage stageSatchmoUnqualified 
                 $ show $ TH.ppr 
                 $ unqualifiedNames thProgram
  return thProgram
