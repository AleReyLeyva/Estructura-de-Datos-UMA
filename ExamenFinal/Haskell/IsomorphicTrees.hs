-------------------------------------------------------------------------------
-- Data Structures. February 2021.
-- Grado en Informática. UMA.
--
-- Student's name: Rey Leyva, Alejandro
-- Student's group: Matemáticas + Informática
-------------------------------------------------------------------------------

module IsomorphicTrees where

import           DataStructures.Graphics.DrawTrees

data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving Show

-- DO NOT MODIFY THE CODE ABOVE

isomorphic :: BinTree a -> BinTree b -> Bool
isomorphic Empty Empty = True 
isomorphic Empty (Node a lt rt) = False 
isomorphic (Node a lt rt) Empty = False 
isomorphic (Node a lt rt) (Node b lt' rt') = isomorphic lt lt' && isomorphic rt rt'

-- DO NOT MODIFY THE CODE BELOW

-- To draw the original tree use:
--
-- >>> drawOn "tree4.svg" tree4

empty :: BinTree a
empty = Empty

singleton :: BinTree Int
singleton = Node 5 Empty Empty

tree1 :: BinTree Int
tree1 = Node 6
               (Node 3 Empty Empty)
               (Node 8 Empty Empty)

tree2 :: BinTree Int
tree2 = Node 8
               (Node 27
                        Empty
                        (Node 3 Empty Empty))
               (Node 15
                        (Node 21 Empty Empty)
                        (Node  6 Empty Empty))

tree2Char :: BinTree Char
tree2Char = Node 'H'
                   (Node 'A'
                           Empty
                           (Node 'S' Empty Empty))
                   (Node 'K'
                          (Node 'E' Empty Empty)
                          (Node 'L' Empty Empty))

tree3 :: BinTree Int
tree3 = Node 9
               (Node 12
                        Empty
                        (Node 23 Empty Empty))
               (Node  6
                        (Node 4
                                (Node 10 Empty Empty)
                                Empty)
                        (Node 7 Empty Empty))

tree4 :: BinTree Int
tree4 = Node 16
                (Node 8
                         (Node 4
                                  Empty
                                  (Node 6 Empty Empty))
                         (Node 12 Empty Empty))
                (Node 32
                         (Node 24
                                  (Node 20 Empty Empty)
                                  Empty)
                         (Node 64 (Node 48 Empty Empty)
                                  (Node 82 Empty Empty)))
                                  

instance Subtrees (BinTree a) where
  subtrees Empty        = []
  subtrees (Node _ l r) = [l, r]

  isEmptyTree Empty = True
  isEmptyTree _     = False

instance Show a => ShowNode (BinTree a) where
  showNode Empty        = ""
  showNode (Node x _ _) = show x
