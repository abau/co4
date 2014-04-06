theory Bin
imports Main
begin

datatype Num = Zero | Bit0 Num | Bit1 Num

fun succ :: "Num => Num" where
   "succ x = (case x of
       Zero => Bit1 Zero
       | Bit0 x => Bit1 x
       | Bit1 x => Bit0 (succ x) )"

fun plus :: "Num => Num => Num" where
  "plus x y = (case x of 
     Zero => y
    | Bit0 x => case y of
        Zero => x
        | Bit0 y => Bit0 (plus x y)
        | Bit1 y => Bit1 (plus x y)
    | Bit1 x => case y of
        Zero => x
        | Bit0 y => Bit1 (plus x y)
        | Bit1 y => Bit0 (succ (plus x y)) )"

datatype Bool = False | True

fun equals :: "Num => Num => Bool" where
   "equals x y = (case x of
       Zero => case y of
           Zero => True
           | Bit0 y => equals x y
           | Bit1 y => False 
      | Bit0 x => case y of
           Zero => equals x y
           | Bit0 y => equals x y
           | Bit1 y => False 
      | Bit1 x => case y of
            Zero => False
           | Bit0 y => False
           | Bit1 y => equals x y )"

export_code plus equals in Haskell file "/tmp/"

end

