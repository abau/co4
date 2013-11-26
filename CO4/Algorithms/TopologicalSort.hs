module CO4.Algorithms.TopologicalSort
  (bindingGroups, adtGroups)
where

import           Data.Graph (stronglyConnComp,flattenSCC)
import           Data.List (nub)
import           CO4.Language 
import           CO4.Names (untypedName)
import           CO4.Algorithms.Free (free)

-- |Computes groups of mutually recursive value declarations that are returned in
-- topological order
bindingGroups :: [Binding] -> [[Binding]]
bindingGroups bindings =
  let graph                  = map toNode bindings
      toNode b@(Binding n e) = (b, untypedName n, map untypedName $ free e)
  in
    map (nub . flattenSCC) $ stronglyConnComp graph

-- |Computes groups of mutually recursive ADT declarations that are returned in
-- topological order
adtGroups :: [Adt] -> [[Adt]]
adtGroups adts = 
  let graph = map toNode adts
      toNode adt@(Adt name _ conss) = 
        (adt, name, nub $ concatMap fromConstructor conss)

      fromConstructor (CCon _ args) = concatMap fromType args
      fromType        (TVar _     ) = []
      fromType        (TCon c ts  ) = c : (concatMap fromType ts)
  in
    map (nub . flattenSCC) $ stronglyConnComp graph
