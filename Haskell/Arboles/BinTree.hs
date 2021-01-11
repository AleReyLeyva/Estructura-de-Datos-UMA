module BinTree where

import           DataStructures.Graphics.DrawTrees
import           Test.QuickCheck

-- árboles binarios en Haskell
data TreeB a = EmptyB
             | NodeB a (TreeB a) (TreeB a) deriving Show
               --  root  left     right

-- aunque tenemos deriving Show, no es muy útil; estas instancias son para
-- pintar árboles binarios en formato gráfico
instance Subtrees (TreeB a) where
    isEmptyTree EmptyB = True
    isEmptyTree _      = False

    subtrees EmptyB        = []
    subtrees (NodeB _ i d) = [i,d]

instance Show a => ShowNode (TreeB a) where
    showNode EmptyB        = ""
    showNode (NodeB r _ _) = show r

-- ejemplos de árboles binarios
tree2 :: TreeB Integer
tree2 =
  NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                   (NodeB 5 EmptyB EmptyB))
          (NodeB 3 (NodeB 6 EmptyB EmptyB)
                    EmptyB)

tree3 :: TreeB Integer
tree3 =
  NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                   (NodeB 5 EmptyB EmptyB))
          (NodeB 3 (NodeB 6 EmptyB EmptyB)
                   (NodeB 2 EmptyB EmptyB))

tree4 :: TreeB Integer
tree4 =
  NodeB 10 (NodeB 20 (NodeB 40 EmptyB EmptyB)
                     (NodeB 50 EmptyB EmptyB))
           (NodeB 30 (NodeB 60 EmptyB EmptyB)
                     (NodeB 20 (NodeB 70 EmptyB EmptyB) EmptyB))

tree5 :: TreeB Char
tree5 = NodeB 'H' (NodeB 'A' (NodeB 'K'
                                       (NodeB 'E' EmptyB EmptyB)
                                       (NodeB 'R' EmptyB EmptyB))
                             (NodeB 'E' EmptyB EmptyB))
                  (NodeB 'S' (NodeB 'L' EmptyB EmptyB)
                             (NodeB 'L' EmptyB EmptyB))

-- para construir otros áboles binarios de ejemplo
leftChild :: TreeB t -> TreeB t
leftChild (NodeB _ i _) = i

rightChild :: TreeB t -> TreeB t
rightChild (NodeB _ _ d) = d

-- peso de un árbol binario
weightB :: TreeB a -> Int
weightB EmptyB = 0
weightB  (NodeB x lt rt) = 1 + weightB lt + weightB rt

-- altura de un árbol binario
heightB :: (Ord a, Num a) => TreeB t -> a
heightB EmptyB = 0
heightB (NodeB x lt rt) = 1 + max (heightB lt) (heightB rt)

-- all nodes at level n
atLevelB :: Integer -> TreeB a -> [a]
atLevelB _ EmptyB = []
atLevelB 0 (NodeB x lt rt) = [x]
atLevelB n (NodeB x lt rt) = atLevelB (n-1) lt ++ atLevelB (n-1) rt

-- all paths from root to node with value x
caminosHasta :: Eq a => a -> TreeB a -> [[a]]
caminosHasta x EmptyB = []
caminosHasta x (NodeB y lt rt)
  | x==y = [y] : ps
  | otherwise = ps
  where
    ps = map (y:) (caminosHasta x lt ++ caminosHasta x rt)

-- all paths from root to node with value x (exhaustive)
pathsToB :: (Eq a) => a -> TreeB a -> [[a]]
pathsToB x EmptyB = []
pathsToB x (NodeB y lt rt)
  | x==y = [y] : ps
  | otherwise = ps
  where
    ps = map (y:) (pathsToB x lt ++ pathsToB x rt)

-- comprobar si x es elemento de un árbol binario
isElemB :: Eq a => a -> TreeB a -> Bool
isElemB _ EmptyB = False 
isElemB a (NodeB x lt rt) = a == x || isElemB a lt || isElemB a rt

-- la frontera es el conjunto de nodos hoja
borderB :: TreeB a -> [a]
borderB EmptyB = []
borderB (NodeB x EmptyB EmptyB) = [x]
borderB (NodeB x EmptyB rt) = borderB rt
borderB (NodeB x lt EmptyB) = borderB lt
borderB (NodeB x lt rt) = borderB lt ++ borderB rt

-- recorridos de un árbol binario
inOrderB :: TreeB a -> [a]
inOrderB EmptyB = []
inOrderB (NodeB x lt rt) = inOrderB lt ++ [x] ++ inOrderB rt