module CO4.Thesis.CaseStandalone where

constraint p u = case p of
  False -> False
  True  -> case u of False -> True
                     True  -> False
