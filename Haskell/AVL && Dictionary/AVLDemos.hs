module AVLDemos where

import qualified DataStructures.SearchTree.AVL as AVL
import qualified DataStructures.SearchTree.BST as BST

frase :: String
frase = "Un AVL es un arbol de busqueda equilidrado en altura"

drawBSTOn :: Show a => FilePath -> BST.BST a -> IO()
drawBSTOn fileName t = BST.drawOnWith fileName show t

drawAVLOn :: Show a => FilePath -> AVL.AVL a -> IO()
drawAVLOn fileName t = AVL.drawOnWith fileName show t

bst :: (Show a, Ord a) => [a] -> IO()
bst xs = drawBSTOn "bst.svg" (BST.mkBST xs)

avl :: (Show a, Ord a) => [a] -> IO()
avl xs = drawAVLOn "avl.svg" (AVL.mkAVL xs)
