theory Mul
imports "~~/src/HOL/Word/Bool_List_Representation"
begin
fun f where
  "f x =
  ( [ True, False, False,True,True] = rbl_mult x x)"
export_code f in Haskell file "/tmp" 
end

