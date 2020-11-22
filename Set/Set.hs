module Set
  ( Set
  , empty
  , isEmpty
  , insert
  , isElem
  , delete
  , union
  , fold
  ) where

import           Test.QuickCheck hiding (sample)

data Set a = Empty
           | Node a (Set a)
           deriving (Show, Eq)

-- invariante: los elementos están ordenados y no hay repetidos
sample :: Set Char
sample = Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))

-- complejidad: O(1)
-- |
-- >>> empty
-- Empty
empty :: Set a
empty = Empty

-- complejidad: O(1)
-- |
-- >>> isEmpty empty
-- True
-- >>> isEmpty sample
-- False
isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty (Node x s) = False

-- complejidad: O(1)
-- |
-- >>> isElem 'x' sample
-- False
-- >>> isElem 'f' sample
-- True
isElem :: Ord a => a -> Set a -> Bool
isElem a Empty = False
isElem a (Node x s)
  | a < x = False
  | otherwise = a == x || isElem a s

-- complejidad: O(n)
-- |
-- >>> insert 'x' sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'x' (Node 'z' Empty))))
-- >>> insert 'a' sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))
insert :: Ord a => a -> Set a -> Set a
insert a Empty = Node a Empty
insert a (Node x s)
  | a < x = Node a (Node x s)
  | a == x = Node x s
  | otherwise = Node x (insert a s)

-- complejidad: O(n)
-- |
-- >>> delete 'x' sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))
-- >>> delete 'f' sample
-- Node 'a' (Node 'c' (Node 'z' Empty))
delete :: Ord a => a -> Set a -> Set a
delete a Empty = Empty
delete a (Node x s)
  | isElem a (Node x s) = if (a == x) then s else Node x (delete a s)
  | otherwise = (Node x s)

-- complejidad: O(n)
-- |
-- > union sample sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))
-- > union sample (insert 'w' (insert 'f' empty))
-- Node 'a' (Node 'c' (Node 'f' (Node 'w' (Node 'z' Empty))))
union :: Ord a => Set a -> Set a -> Set a
union s Empty = s
union Empty s = s
union (Node x xs) (Node y ys)
  | isElem x (Node y ys) = union xs (Node y ys)
  | otherwise = union xs (insert x (Node y ys))

-- complejidad: O(n)
fold :: (a -> b -> b) -> b -> Set a -> b
fold f z s = recSet s
 where
  recSet Empty      = z
  recSet (Node x s) = f x (recSet s)

-- La instancia de Arbitrary es para enseñar a QuickCheck a
-- generar Queue aleatorias: no hay que saber cómo hacerlo;
-- siempre se facilita

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary =  do
                xs <- listOf arbitrary
                return (foldr insert empty xs)
