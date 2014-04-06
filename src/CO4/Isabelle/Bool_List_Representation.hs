{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Bool_List_Representation where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified List;
import qualified HOL;

rbl_succ :: [Bool] -> [Bool];
rbl_succ [] = [];
rbl_succ (x : xs) = (if x then False : rbl_succ xs else True : xs);

rbl_add :: [Bool] -> [Bool] -> [Bool];
rbl_add [] x = [];
rbl_add (y : ys) x =
  let {
    ws = rbl_add ys (List.tl x);
  } in not (y == List.hd x) : (if List.hd x && y then rbl_succ ws else ws);

rbl_mult :: [Bool] -> [Bool] -> [Bool];
rbl_mult [] x = [];
rbl_mult (y : ys) x =
  let {
    ws = False : rbl_mult ys x;
  } in (if y then rbl_add ws x else ws);

}
