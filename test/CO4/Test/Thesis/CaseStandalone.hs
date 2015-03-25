module CO4.Test.Thesis.CaseStandalone where

constraint p u = case p of
  False -> False
  True  -> case u of False -> True
                     True  -> False
