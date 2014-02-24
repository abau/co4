module CO4.Test.TermComp2014.PPrint
where

import Data.Char (chr)
import Data.List (intercalate,intersperse)
import CO4.Util (fromBinary)
import CO4.Test.TermComp2014.Standalone
import CO4.Test.TermComp2014.Trs

pprintLabeledTrs :: Int -> Trs -> Model -> String
pprintLabeledTrs n (Trs rules) model = unlines $ do
  r <- rules
  s <- sigmas
  return $ goRule r s
  where
    sigmas = assignments n $ Trs rules

    goRule (Rule l r) sigma = concat [ goTerm l sigma
                                     , " -> "
                                     , goTerm r sigma ]

    goTerm (Var v)       sigma = concat [ pprintSymbol v, "^", pprintValue l ]
      where l = valueOfVar v sigma
    goTerm (Node s args) sigma = concat [ pprintSymbol s, "^", pprintValue l, " ("
                                        , intercalate ", " (map (flip goTerm sigma) args), ")" ]
      where l = valueOfTerm model sigma 
              $ Node s args

pprintSymbol :: Symbol -> String
pprintSymbol = return . chr . fromBinary

pprintValue :: Domain -> String
pprintValue = show . fromBinary

pprintModel :: Model -> String
pprintModel = unlines . intersperse "" . map pprintInterpretation

pprintInterpretation :: (Symbol,Interpretation) -> String
pprintInterpretation (s,i) = unlines $ map (pprintMapping $ pprintSymbol s) i

pprintMapping :: String -> Mapping -> String
pprintMapping s (xs, y) = 
  concat [ s, " ", intercalate " " (map pprintValue xs), " |-> ", pprintValue y ]
