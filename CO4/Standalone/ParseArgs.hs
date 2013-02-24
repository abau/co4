module CO4.Standalone.ParseArgs
  (parseArgs)
where

import           Control.Monad (when)
import           System.Environment (getArgs)
import           System.Console.GetOpt
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           CO4.Config (Config(..),Configs,defaultInstantiationDepth)
import           CO4.Compilation (stageNames)

configurations :: [OptDescr (Configs -> Configs)]
configurations = 
  [ Option ['v'] ["verbose"] (NoArg $ (:) Verbose) "Log verbosely"
  {-
  , Option ['d'] ["degree"]
      (ReqArg (\d -> (:) (Degree $ read d)) "DEGREE") "Degree of Raml's analysis"
  , Option [] ["degree-loop"]
      (ReqArg (\d -> (:) (DegreeLoop $ read d)) "DEGREE") "Raml analyses up to degree DEGREE (inclusive)"
  , Option ['m'] ["metric"]
      (ReqArg (\m -> (:) (Metric m)) "METRIC") ("Metric of Raml's analysis: " ++ show metrics)
  , Option [] ["no-raml"] (NoArg $ (:) NoRaml) "No Raml analysis"
  , Option [] ["no-satchmo"] (NoArg $ (:) NoSatchmo) "No Satchmo code generation"
  -}
  , Option [] ["dump-after"]
      (ReqArg (\s -> (:) (parseDumpAfter s)) "STAGE[.FILE]") ("Dump code. STAGE=" ++ show stageNames)
  , Option [] ["dump-all"]
      (OptArg (\fp -> (:) (DumpAll $ fromMaybe "" fp)) "[FILE]") ("Dump code of all intermediate stages. Omit FILE for stdout.")
  , Option [] ["instantiation-depth"]
      (ReqArg (\i -> (:) (InstantiationDepth $ read i)) "DEPTH") ("Maximum instantiation depth (default: " ++ show defaultInstantiationDepth ++ ")")
  , Option ['i'] ["import-prelude"] (NoArg $ (:) ImportPrelude) "Import prelude"
  , Option ['k'] ["keep-tmp"] (NoArg $ (:) KeepTmp) "Do not remove temporary files"
  , Option ['u'] ["undefined-size"]
      (ReqArg (\s -> (:) (UndefinedSize s)) "STRING") "Size of undefined data"
  ]

  where
    parseDumpAfter string = case break (== '.') string of
      (stage,[])   -> checkStage stage $ DumpAfter stage ""
      (stage,rest) -> checkStage stage $ DumpAfter stage $ tail rest

      where checkStage s = if s `elem` stageNames then id 
                           else error $ concat [ "Unknown stage '",s,"'. "
                                               , "Available stages: ", show stageNames
                                               ]
parseArgs :: IO (Configs, FilePath)
parseArgs = do
  args <- getArgs
  case getOpt RequireOrder configurations args of
    (o,fp:n,[]) ->
      do when (not $ null n) $ 
            putStrLn $ "Omitting arguments " ++ (intercalate "," n)
         return $ (foldr ($) [] o, fp)
    (_,[],errors) -> error $ concat errors ++ usageInfo "No input file" configurations
    (_,_,errors)  -> error $ concat errors ++ usageInfo "" configurations
