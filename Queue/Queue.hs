module Queue(Queue,
             isEmpty,
             empty,
             enqueue,
             first,
             dequeue
             ) where

import           Test.QuickCheck

data Queue a = Empty
             | Node a (Queue a)
             deriving (Eq, Show)

customers :: Queue String
--                first                       last
customers = Node "peter" (Node "mary" (Node "john" Empty))

-- Complejidad: O(1)
-- |
-- >>> isEmpty empty
-- True
-- >>> isEmpty customers
-- False
isEmpty :: Queue a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Complejidad: O(1)
-- |
-- >>> empty
-- Empty
empty :: Queue a
empty = Empty

-- Complejidad: O(1)
-- |
-- >>> first customers
-- "peter"
first :: Queue a -> a
first (Node x xs) = x

-- Complejidad: O(1)
-- |
-- >>> dequeue customers
-- Node "mary" (Node "john" Empty)
-- >>> dequeue (dequeue customers)
-- Node "john" Empty
dequeue :: Queue a -> Queue a
dequeue (Node x xs) = xs

-- Complejidad: O(?)
-- |
-- >>> enqueue "frank" customers
-- Node "peter" (Node "mary" (Node "john" (Node "frank" Empty)))
-- >>> enqueue "nicole" (enqueue "frank" customers)
-- Node "peter" (Node "mary" (Node "john" (Node "frank" (Node "nicole" Empty))))
enqueue :: a -> Queue a -> Queue a
enqueue a Empty = Node a Empty
enqueue a (Node x xs) = Node x (enqueue a xs)

-- La instancia de Arbitrary es para enseñar a QuickCheck a
-- generar Queue aleatorias: no hay que saber cómo hacerlo;
-- siempre se facilita

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary =  do
                xs <- listOf arbitrary
                return (foldr enqueue empty xs)