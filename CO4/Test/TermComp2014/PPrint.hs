module CO4.Test.TermComp2014.PPrint
where

import           Data.Char (chr)
import           Data.List (intercalate,intersperse,sortBy,groupBy)
import           Data.Function (on)
import           CO4.Util (fromBinary)
import           CO4.PreludeNat (value)
import           CO4.Test.TermComp2014.Standalone 
import           CO4.Test.TermComp2014.Util (dpToTrs)

pprintUnlabeledTrs :: UnlabeledTrs -> String
pprintUnlabeledTrs = pprintTrs pprintSymbol pprintSymbol $ const ""

pprintLabeledTrs :: (label -> String) -> Trs Symbol Symbol label -> String
pprintLabeledTrs = pprintTrs pprintSymbol pprintSymbol

pprintDPTrs :: (l -> String) -> DPTrs l -> String
pprintDPTrs goL = pprintTrs pprintSymbol pprintMarkedSymbol goL . dpToTrs

pprintDPRule :: (l -> String) -> DPRule l -> String
pprintDPRule goL = pprintRule pprintSymbol pprintMarkedSymbol goL

pprintTrs :: (v -> String) -> (n -> String) -> (l -> String) -> Trs v n l -> String 
pprintTrs goV goN goL (Trs rules) = unlines $ map (pprintRule goV goN goL) rules

pprintRule :: (v -> String) -> (n -> String) -> (l -> String) -> Rule v n l -> String 
pprintRule goV goN goL (Rule l r) = concat [ goTerm l, " -> ", goTerm r ]
  where
    goTerm (Var v)         = goV v
    goTerm (Node s l args) = 
      case goL l of
        "" -> concat [ goN s          ," (", intercalate ", " (map goTerm args), ")" ]
        l' -> concat [ goN s, "^", l' ," (", intercalate ", " (map goTerm args), ")" ]

pprintValue :: Domain -> String
pprintValue [] = "0"
pprintValue d  = show $ fromBinary d

pprintSymbol :: Symbol -> String
pprintSymbol = return . chr . fromBinary

pprintMarkedSymbol :: MarkedSymbol -> String
pprintMarkedSymbol (s,b) = case b of
  False -> pprintSymbol s
  True  -> pprintSymbol s ++ "#"

pprintModel :: (s -> String) -> Model s -> String
pprintModel f = unlines . intersperse "" . map pprintInterpretation
  where
    pprintInterpretation (s,i) = unlines $ map (pprintMapping $ f s) i
      where
        pprintMapping s (xs, y) = 
          concat [ s, " ", intercalate " " (map pprintValue xs), " |-> ", pprintValue y ]

pprintLabel :: Label -> String
pprintLabel vs = "[" ++ (intercalate ", " $ map pprintValue vs) ++ "]"

pprintPrecedence :: (s -> String) -> (l -> String) -> Precedence s l -> String
pprintPrecedence goS goL = intercalate " > "
                         . map     (intercalate " = " . map fst)
                         . groupBy ((==)    `on` snd)
                         . reverse
                         . sortBy  (compare `on` snd)
                         . map     (\((s,l),n) -> (goS s ++ "^" ++ goL l, value n))
