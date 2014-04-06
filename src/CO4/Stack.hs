module CO4.Stack 
  ( StackTrace, Stack, CallStackTrace, CallStack
  , trace, depth, emptyStack, pushToStack, popFromStack)
where

import Control.Exception (assert)

type StackTrace s = [s]

data Stack s = Stack { trace :: StackTrace s
                     , depth :: Int
                     }
                     deriving Show

type CallStackTrace = StackTrace String
type CallStack      = Stack String

emptyStack :: Stack s
emptyStack = Stack [] 0

pushToStack :: s -> Stack s -> Stack s
pushToStack s stack = 
  stack { trace = s : trace stack
        , depth = 1 + depth stack
        }

popFromStack :: Stack s -> Stack s
popFromStack stack = assert (depth stack > 0) $
  stack { trace = tail $ trace stack
        , depth = depth stack - 1
        }
