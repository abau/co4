import           Control.Monad.IO.Class 
import           System.Directory (getTemporaryDirectory,removeFile)
import           System.FilePath ((</>),(<.>),takeFileName)
import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Quasi)
import           Data.Char (isLetter,toUpper)
import           CO4.Frontend.HaskellSrcExts ()
import           CO4.Compilation (compile)
import qualified CO4.Config as C
import           CO4.Config (MonadConfigurable,logWhenVerbose,whenNot')
import           CO4.Standalone.ParseArgs (parseArgs)
import           CO4.Standalone.Interpreter (interpret)

main :: IO ()
main = do
  (configs, file) <- parseArgs
  input           <- readFile file
  case HE.parseModule input of
    HE.ParseFailed loc msg -> error $ "Standalone.Main: parse error at '" 
                                   ++ show loc ++ "': " ++ msg
    HE.ParseOk p           -> C.configurable configs $ process file p
    
process :: (MonadIO m,MonadConfigurable m,Quasi m) => FilePath -> HE.Module -> m ()
process inputFile (HE.Module loc name pragmas warnings exports imports decls) = do
  p <- compile (HE.Module loc name [] warnings exports imports decls)

  undefinedSize <- getUndefinedSize
  pFile         <- writeToTmp inputFile $ source undefinedSize
                                        $ show $ TH.ppr p

  logWhenVerbose $ "Generating unknowns of size: " ++ undefinedSize
  -- liftIO (interpret pFile (moduleName ++ ".result") >>= putStrLn)
  liftIO (interpret pFile (moduleName ++ ".result") :: IO ())
  whenNot' C.KeepTmp $ do
    logWhenVerbose $ "Deleting file " ++ pFile
    liftIO $ removeFile pFile

  where
    getUndefinedSize = do
      fromCmdLine <- C.fromConfigs C.undefinedSize 
      return $
        case (fromCmdLine, pragmas) of
          (Just s, _)                                                       -> s
          (Nothing, [ HE.OptionsPragma _ (Just (HE.UnknownTool "CO4")) s ]) -> s

          (Nothing, []) -> error $ "Standalone.Main: missing size of unknown data"
          (Nothing, _ ) -> error $ "Standalone.Main: invalid pragmas:\n" 
                                ++ (unlines $ map HE.prettyPrint pragmas)

    moduleName = 
      let (x:xs) = filter isLetter inputFile
      in
        toUpper x : xs

    source undefinedSize compiled = 
      unlines [ "{-# LANGUAGE TemplateHaskell #-}"
              , "{-# LANGUAGE NoMonomorphismRestriction #-}"
              , "{-# LANGUAGE MultiParamTypeClasses #-}"
              , "{-# LANGUAGE FlexibleInstances #-}"
              , "{-# LANGUAGE GADTs #-}"
              , "{-# LANGUAGE FlexibleContexts #-}"
              , "{-# LANGUAGE ScopedTypeVariables #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE NoImplicitPrelude #-}"
              , "{-# LANGUAGE StandaloneDeriving #-}"
              , "{-# LANGUAGE UndecidableInstances #-}"
              , "module " ++ moduleName ++ " where"
              , "import qualified Data.Maybe"
              , "import qualified GHC.Base"
              , "import qualified GHC.Err"
              , "import qualified GHC.Show"
              , "import qualified GHC.Types"
              , "import qualified Satchmo.Code"
              , "import qualified Satchmo.SAT.Mini"
              , "import qualified CO4.EncodedAdt"
              , "import qualified CO4.AdtIndex"
              , "import qualified CO4.Algorithms.Eitherize.UnsizedAdt"
              , "import           CO4.Algorithms.Eitherize.Util"
              , "import qualified CO4.Algorithms.Eitherize.Solve"
              , compiled
              , concat [ "result = CO4.Algorithms.Eitherize.Solve.solveAndTest "
                       , "(GHC.Err.undefined :: ("
                       , undefinedSize 
                       , ")) encMain main "
                       {-
                       , "GHC.Base.>>= GHC.Base.return "
                       , "GHC.Base..   GHC.Show.show"
                       -}
                       ]
              ]

writeToTmp :: (MonadIO m,MonadConfigurable m) => FilePath -> String -> m FilePath
writeToTmp inputFile content = do
  tmpDir <- liftIO getTemporaryDirectory

  let tmpFilePath = tmpDir </> ("co4-compiled-" ++ (takeFileName inputFile)) <.> "hs"

  logWhenVerbose $ "Writing to " ++ tmpFilePath
  liftIO $ writeFile tmpFilePath content
  return tmpFilePath
