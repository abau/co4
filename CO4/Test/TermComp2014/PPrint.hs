module CO4.Test.TermComp2014.PPrint
where

import           Data.Char (chr)
import           Data.List (intercalate,intersperse)
import qualified Data.Map as M
import           Unsafe.Coerce
import           CO4.Util (fromBinary)
import           CO4.Test.TermComp2014.Data
import           CO4.Test.TermComp2014.Util
import           CO4.Test.TermComp2014.SL.Standalone (valueOfVar,valueOfTerm)

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

    goTerm (Var v)       sigma = pprintLabeledSymbol v l 
      where l = unsafeCoerce $ valueOfVar (unsafeCoerce v) (unsafeCoerce sigma)

    goTerm (Node s args) sigma = pprintLabeledSymbol s l 
                              ++ (concat [" (", intercalate ", " (map (flip goTerm sigma) args), ")" ])

      where l = unsafeCoerce $ valueOfTerm (unsafeCoerce model) (unsafeCoerce sigma)
                             $ unsafeCoerce
                             $ Node s args

pprintLabeledSymbol :: Symbol -> Domain -> String
pprintLabeledSymbol s v = concat [pprintSymbol s, "^", pprintValue v]

pprintValue :: Domain -> String
pprintValue = show . fromBinary

pprintSymbol :: Symbol -> String
pprintSymbol = return . chr . fromBinary

pprintModel :: Model -> String
pprintModel = unlines . intersperse "" . map pprintInterpretation
  where
    pprintInterpretation (s,i) = unlines $ map (pprintMapping $ pprintSymbol s) i
      where
        pprintMapping s (xs, y) = 
          concat [ s, " ", intercalate " " (map pprintValue xs), " |-> ", pprintValue y ]

pprintPrecedence :: Precedence -> String
pprintPrecedence = unwords . map go
  where
    go (s,n) = pprintSymbol s ++ "[" ++ (show $ fromNat n) ++ "]"

pprintPrecedence' :: M.Map Symbol (Symbol,Domain) -> Precedence -> String
pprintPrecedence' mapping = unwords . map go
  where
    go (s,n) = case M.lookup s mapping of
      Nothing     -> error "pprintPrecedence'"
      Just (s',v) -> pprintLabeledSymbol s' v ++ "[" ++ (show $ fromNat n) ++ "]"
