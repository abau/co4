module CO4.Stack 
  (StackTrace, Stack, trace, depth, emptyStack, pushToStack, popFromStack)
where

import Control.Exception (assert)

type StackTrace = [String]

data Stack = Stack { trace :: StackTrace
                   , depth :: Int
                   }
                   deriving Show

emptyStack :: Stack
emptyStack = Stack [] 0

pushToStack :: String -> Stack -> Stack
pushToStack name stack = 
  stack { trace = name : trace stack
        , depth = 1 + depth stack
        }

popFromStack :: Stack -> Stack 
popFromStack stack = assert (depth stack > 0) $
  stack { trace = tail $ trace stack
        , depth = depth stack - 1
        }
