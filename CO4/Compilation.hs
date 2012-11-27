{-# LANGUAGE ScopedTypeVariables #-}
module CO4.Compilation
  (Config(..), Configs, compile, metrics, defaultInstantiationDepth, stageNames)
where

import           Control.Exception (catch,ErrorCall(..))
import           Control.Monad.Reader hiding (liftIO)
import           Control.Applicative ((<$>))
import qualified Raml.RAMLTypes as RamlT
import qualified Raml.SimpleTypes as RamlST
import qualified Raml.Ast as RamlA
import qualified Raml.PPrint as RamlPP
import qualified Language.Haskell.TH as TH
import           CO4.Language (Program)
import           CO4.Unique (Unique,UniqueT,runUniqueT,mapUnique)
import           CO4.THUtil (unqualifiedNames)
import           CO4.Frontend
import           CO4.Backend
import           CO4.Backend.TH ()
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

type Stage   = String
data Config  = Verbose
             | Degree Int
             | DegreeLoop Int
             | Metric String
             | NoRaml
             | NoSatchmo
             | DumpAfter Stage FilePath
             | DumpAll FilePath
             | InstantiationDepth Int
             deriving (Eq)

type Configs      = [Config]
type Configurable = ReaderT Configs (UniqueT IO)

compile :: (ProgramFrontend a) => a -> Configs -> IO [TH.Dec]
compile a configs = do
  runUniqueT $ flip runReaderT configs $ do
    parsedProgram <- liftUnique $ parsePreprocessedProgram a 

    uniqueProgram <-  dumpAfterStage' stageParsed parsedProgram

                  >>= liftUnique      .    uniqueLocalNames 
                  >>= dumpAfterStage' stageUniqueLocalNames

                  >>= liftUnique      . schemes
                  >>= dumpAfterStage'   stageTypeInference
                  >>= return          . eraseTypedNames

                  >>= liftUnique      .    etaExpansion
                  >>= dumpAfterStage' stageEtaExpansion

                  >>= liftUnique      .    globalize 
                  >>= dumpAfterStage' stageGlobalize

                  >>= liftUnique      .    saturateApplication
                  >>= dumpAfterStage' stageSaturateApplication

                  >>= liftUnique      .    instantiation (instantiationDepth configs)
                  >>= dumpAfterStage' stageInstantiation

    whenNot' NoRaml $ compileToRaml uniqueProgram >>= analyseRaml (degree configs)

    noSatchmo <- is NoSatchmo
    result    <- if noSatchmo then return [] else compileToSatchmo uniqueProgram

    logWhenVerbose "Compilation successful"
    return $ displayProgram parsedProgram ++ result
  where
    m                             = metric configs
    dumpAfterStage' stage program = 
      dumpAfterStage stage (displayProgram program) >> return program

    analyseRaml d (decs,main) =
      case RamlST.analyseDecs decs of
        Left msg    -> error $ "Raml.analyseDecs: " ++ msg
        Right decs' -> case RamlST.typeProgram decs' main of
          Left msg                     -> error $ "Raml.typeProgram: " ++ msg
          Right (typedDecs, typedMain) -> do
            logWhenVerbose $ "Raml is analysing using degree " ++ show d ++ " ..."
            c <- liftIO $ ( do r <- RamlA.analyseAST typedMain typedDecs m d
                               seq r $ return r )
                    `catch`
                    (\(ErrorCall msg) -> do putStrLn $ "Raml aborted: " ++ msg
                                            return False
                    )
            if c
             then do
               logWhenVerbose $ "Raml successfully finished analysis of degree " ++ show d
               return ()
             else case isDegreeLoop configs of
                    Nothing  -> error $ "Raml can not infer degree " ++ show d
                    Just max | d < max -> analyseRaml (d + 1) (decs,main) 
                    Just max -> error $ "Raml can not infer degree up to " ++ show max

compileToRaml :: Program -> Configurable RamlT.Program
compileToRaml _ = error "Raml compilation is out of order"

{-
do        
  instDepth <- fromConfigs instantiationDepth
  logWhenVerbose $ "Instantiation using depth " ++ show instDepth
  ramlP     <- liftUnique $ globalize p
                       >>= schemes (HMConfig IntroduceVarTLamTApp) emptyContext 
                       >>= instantiation instDepth
                       >>= displayPreprocessedRamlProgram 

  let stringRamlP = show $ RamlPP.pprint ramlP

  dumpWhenConfig isDumpRaml "Raml" stringRamlP
  return ramlP
  -}

compileToSatchmo :: Program -> Configurable [TH.Dec]
compileToSatchmo program = do
  thProgram <- liftUnique $ eitherize program

  dumpAfterStage stageSatchmo $ show $ TH.ppr thProgram
  dumpAfterStage stageSatchmoUnqualified $ show $ TH.ppr $ unqualifiedNames thProgram
  return thProgram

liftUnique :: Unique a -> Configurable a
liftUnique = lift . mapUnique

liftIO :: IO a -> Configurable a
liftIO = lift . lift

logWhenVerbose :: String -> Configurable ()
logWhenVerbose msg = when' Verbose $ liftIO $ putStrLn msg

dumpAfterStage :: Stage -> String -> Configurable ()
dumpAfterStage stage content = do
  stageDump <- fromConfigs $ isStageDump stage
  case stageDump of
    Just filePath -> liftIO $ dump stage content filePath 
    Nothing       -> return ()

dump :: String -> String -> FilePath -> IO ()
dump title content filePath = case filePath of
  "" -> putStrLn $ unwords [ "##", title, replicate (30 - length title) '#', "\n"
                           , content]
  _  -> writeFile filePath content


is :: Config -> Configurable Bool
is c = elem c <$> ask

when' :: Config -> Configurable () -> Configurable ()
when' c doThis = is c >>= \b -> if b then doThis else return ()

whenNot' :: Config -> Configurable () -> Configurable ()
whenNot' c doThis = is c >>= \b -> if not b then doThis else return ()

fromConfigs :: (Configs -> a) -> Configurable a
fromConfigs f = ask >>= return . f

degree :: Configs -> Int
degree cs = case cs of
  (Degree d):_ -> d
  []           -> error "Compilation: No degree provided"
  _            -> degree $ tail cs

metrics :: [String]
metrics = [ "heap-space", "eval-steps", "overflow", "cost-free" ]
metric :: Configs -> RamlT.ResourceMetric
metric cs = case cs of
  (Metric "heap-space"):_ -> RamlT.heapSpace
  (Metric "eval-steps"):_ -> RamlT.evalSteps
  (Metric "overflow"):_   -> RamlT.overflow
  (Metric "cost-free"):_  -> RamlT.costFree
  (Metric m):_            -> error $ concat 
                              [ "Compilation: Unknown metric '", m, "'. "
                              , "Available metrics: ", show metrics 
                              ]
  []                      -> error "Compilation: No metric provided"
  _                       -> metric $ tail cs

isDegreeLoop :: Configs -> Maybe Int
isDegreeLoop cs = case cs of
  (DegreeLoop d):_ -> Just d
  []               -> Nothing
  _                -> isDegreeLoop $ tail cs

defaultInstantiationDepth = 10
instantiationDepth :: Configs -> Int
instantiationDepth cs = case cs of
  (InstantiationDepth d):_ -> d
  []                       -> defaultInstantiationDepth
  _                        -> instantiationDepth $ tail cs

isStageDump :: Stage -> Configs -> Maybe FilePath
isStageDump stage cs = case cs of
  (DumpAfter s fp):_ | s == stage -> Just fp
  (DumpAll         fp):_          -> Just fp
  []                              -> Nothing
  _                               -> isStageDump stage $ tail cs
