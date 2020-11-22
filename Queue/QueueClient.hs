
module QueueClient where

import           Queue
import           Stack

stack1 :: Stack Integer
stack1 = push 1 (push 2 (push 3 (push 4 Stack.empty)))

sSize :: Stack a -> Integer
sSize s
  | Stack.isEmpty s = 0
  | otherwise = 1 + sSize (pop s)

queue1 :: Queue String
queue1 = enqueue "c++" (enqueue "java" (enqueue "haskell" Queue.empty))

qSize :: Queue a -> Integer
qSize q
  | Queue.isEmpty q = 0
  | otherwise = 1 + qSize (dequeue q)
