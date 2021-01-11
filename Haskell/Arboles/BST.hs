module BST where

import           Data.Maybe                        (isJust)
import           DataStructures.Graphics.DrawTrees
import           Test.QuickCheck

-- árboles binarios de búsqueda en Haskell
data BST a = Empty
           | Node a (BST a) (BST a) deriving Show
            --  root left   right

-- aunque tenemos deriving Show, no es muy útil; estas instancias son para
-- pintar árboles binarios en formato gráfico
instance Subtrees (BST a) where
    isEmptyTree Empty = True
    isEmptyTree _     = False

    subtrees Empty        = []
    subtrees (Node _ i d) = [i,d]

instance Show a => ShowNode (BST a) where
    showNode Empty        = ""
    showNode (Node r _ _) = show r


inOrder :: BST a -> [a]
inOrder Empty        = []
inOrder (Node x i d) = inOrder i ++ x : inOrder d

mkBST :: Ord a => [a] -> BST a
mkBST xs = foldr insert empty xs

treeSort :: Ord a => [a] -> [a]
treeSort xs = (inOrder . mkBST) xs

empty :: BST a
empty = Empty

insert :: Ord a => a -> BST a -> BST a
insert a Empty = Node a Empty Empty
insert a (Node x lt rt)
  | a == x = Node a lt rt
  | a < x = Node x (insert a lt) rt
  | otherwise = Node x lt (insert a rt) 


search :: Ord a => a -> BST a -> Maybe a
search a Empty = Nothing 
search a (Node x lt rt)
  | a == x = Just x
  | a < x = search a lt
  | otherwise = search a rt

isElem :: Ord a => a -> BST a -> Bool
isElem a Empty = False 
isElem a (Node x lt rt)
  | a == x = True 
  | a < x = isElem a lt
  | otherwise = isElem a rt
