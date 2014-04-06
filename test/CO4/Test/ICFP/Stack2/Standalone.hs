-- | idea: the program (tree) is represented 
-- as a list of statements for a stack machine.

-- difference to .Stack.  :  list of stacks is statically allocated from the outside

module CO4.Test.ICFP.Stack2.Standalone where

import CO4.Prelude
import CO4.PreludeNat

data Program = Program [Statement] [[Stack]] deriving (Show)

data Stack = Stack Nat -- ^ depth (length of valid prefix of contents)
                   [ Number ] -- ^ contents
    deriving Show

constraint :: [(Number,Number)] -> Program -> Bool
constraint samples p = case p of
    Program bs stackss -> all (\ (xy,stacks) -> case xy of (x,y) -> computation_ok bs (x,stacks,y) ) 
                           ( zip samples stackss )

data Statement 
       = Op0 Op0
       | Op1 Op1 
       | Op2 Op2 
       | Op3 Op3
       deriving (Show)

-- | these push one value (increase stack length)
data Op0 = Null | One | Id
     deriving ( Show )

-- | these pop one, and push one value (keep stack length)
data Op1 = Nop -- ^ since we fix the total program length
              -- we may need no-ops as fillers 
         | Not | Shl1 | Shr1 | Shr4 | Shr16 
     deriving (Show)

-- | these pop two values, and push one value (decrease stack length)
data Op2 = And | Or | Xor | Plus deriving (Show)

data Op3 = If0 deriving (Show)


type Number = Nat

type Env = Number


-- NOTE for allocation:
-- number of stacks is one more than number of statements.
-- the first stack should have size 0.
-- (representing the state before the first program statement)
-- last stack should contain the result (have size 1).
-- numbers for stack depth have bit width 2 (HACK!)
computation_ok :: [Statement] -> (Number, [Stack], Number) -> Bool
computation_ok bs (x, stacks, y) = 
    case assertKnown stacks of
        [] -> False
        s : ss -> case s of
            Stack sz cont -> isZeroNat sz && check_computation x bs s ss y

check_computation x bs s0 ss y = case assertKnown bs of
    [] -> case s0 of 
          Stack sz cont -> eqNat (nat 3 1) sz && eqNat (head cont) y
    b : bs' -> case assertKnown ss of
        [] -> False
        s1 : ss' -> check_step x s0 b s1 && check_computation x bs' s1 ss' y

check_step env s0 b s1 = case s0 of
    Stack sz0 cont0 -> case s1 of
        Stack sz1 cont1 -> (geNat sz1 (nat 3 1)) && case b of
            Op0 op0 -> eqNat (plusNat sz0 (nat 3 1)) sz1
                && equalListNat cont0 (tail cont1)
                && eqNat (head cont1) ( case op0 of
                          Null -> nat 64 0 
                          One  -> nat 64 1 
                          Id   -> env 
                                      )
            Op1 op1 -> eqNat                  sz0 sz1
                && equalListNat (tail cont0) (tail cont1)
                && eqNat (head cont1) ( 
                         let arg = head cont0 
                         in case op1 of
                           Nop -> arg
                           Not -> invertNat arg
                           Shl1  -> shiftLNat   arg
                           Shr1  -> shiftRNat   arg
                           Shr4  -> shiftR4Nat  arg
                           Shr16 -> shiftR16Nat arg
                                      )
            Op2 op2 -> eqNat                  sz0 (plusNat sz1 (nat 3 1))
                && equalListNat (tail (tail cont0)) (tail cont1)
                && eqNat (head cont1) (  
                         let arg2 = head cont0
                             arg1 = head (tail cont0)
                         in  case op2 of
                              And  -> andNat   arg1 arg2
                              Or   -> orNat    arg1 arg2
                              Xor  -> xorNat   arg1 arg2
                              Plus -> plus'Nat arg1 arg2
                                      )
            Op3 op3 -> eqNat                  sz0 (plusNat sz1 (nat 3 2))
                && equalListNat (tail (tail (tail cont0))) (tail cont1)
                && eqNat (head cont1) ( 
                         let arg3 = head cont0
                             arg2 = head (tail cont0)
                             arg1 = head (tail (tail cont0))
                         in  case op3 of
                              If0 -> case isZeroNat arg1 of
                                  True  -> arg2
                                  False -> arg3
                                      )

-- and $ zipWith (==) xs ys
-- (ignoring extra elements in case list lengths differ)
equalListNat xs ys =
    case assertKnown xs of
        [] -> True
        x : xs' -> case assertKnown ys of
            [] -> True
            y : ys' -> eqNat x y && equalListNat xs' ys'


-----------------------------------------------------------------------------------------------

orig_constraint :: [(Number,Number)] -> Program -> Bool
orig_constraint samples p = case p of Program bs stackss -> all (\(x,y) -> eqNat y (eval bs x)) samples

-- test case (for Standalone evaluation)
-- eval (Program [ Op0 One, Op0 Id, Op2 Plus  ]) (nat 64 3)  ==> (nat 64 4)

eval :: [Statement] -> Number -> Number
eval bs x = eval_stack x bs []

shiftR4Nat  x = shiftRNat (shiftRNat (shiftRNat (shiftRNat x)))
shiftR16Nat x = shiftR4Nat (shiftR4Nat (shiftR4Nat (shiftR4Nat x)))

eval_stack :: Number-> [Statement] -> [Number] -> Number
eval_stack env bs stack = case assertKnown bs of
    [] -> case stack of -- must have length 1 
        [] -> undefined
        y : ys -> case ys of
            [] -> y
            z : zs -> undefined
    b : bs' -> 
        let stack' = case b of
              Op0 op0 -> stack
              Op1 op1 -> tail stack
              Op2 op2 -> tail (tail stack)
              Op3 op3 -> tail (tail (tail stack))
            res = case b of
              Op0 op0 -> case op0 of
                          Null -> nat 64 0 
                          One  -> nat 64 1 
                          Id   -> env
              Op1 op1 -> let arg = head stack
                         in  case op1 of
                           Nop -> arg
                           Not -> invertNat arg
                           Shl1  -> shiftLNat   arg
                           Shr1  -> shiftRNat   arg
                           Shr4  -> shiftR4Nat  arg
                           Shr16 -> shiftR16Nat arg
              Op2 op2 -> let arg2 = head stack
                             arg1 = head (tail stack)
                         in  case op2 of
                              And  -> andNat   arg1 arg2
                              Or   -> orNat    arg1 arg2
                              Xor  -> xorNat   arg1 arg2
                              Plus -> plus'Nat arg1 arg2
              Op3 op3 -> let arg3 = head stack
                             arg2 = head (tail stack)
                             arg1 = head (tail (tail stack))
                         in  case op3 of
                              If0 -> case isZeroNat arg1 of
                                  True  -> arg2
                                  False -> arg3
        in  eval_stack env bs' ( res : stack' )
