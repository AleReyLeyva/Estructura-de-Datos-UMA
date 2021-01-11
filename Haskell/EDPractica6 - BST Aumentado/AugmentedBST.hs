-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática. UMA.
-- Práctica 6 - Árboles binarios de búsqueda aumentados
--
-- APELLIDOS, NOMBRE: Rey Leyva, Alejandro
-------------------------------------------------------------------------------

module DataStructures.SearchTree.AugmentedBST
  ( ABST
  , empty               -- :: ABST a
  , isEmpty             -- :: ABST a -> Bool
  , isElem              -- :: (Ord a) => a -> ABST a -> Bool
  , search              -- :: (Ord a) => a -> ABST a -> Maybe a
  , minim               -- :: ABST a -> a
  , maxim               -- :: ABST a -> a

  , insert              -- :: (Ord a) => a -> ABST a -> ABST a
  , deleteMin           -- :: (Ord a) => ABST a -> ABST a
  , delete              -- :: (Ord a) => a -> ABST a -> ABST a

  , select              -- :: Int -> ABST a -> Maybe a
  , floor               -- :: Ord a => a -> ABST a -> Maybe a
  , rank                -- :: Ord a => a -> ABST a -> Int

  , mkABST              -- :: (Ord a) => [a] -> ABST a
  , minABST             -- :: [a] -> ABST a

  , inOrder             -- :: ABST a -> [a]
  , preOrder            -- :: ABST a -> [a]
  , postOrder           -- :: ABST a -> [a]

  , pretty
  , drawOnWith

  ) where

import           Data.List                         (nub, sort)
import           Data.Maybe                        (fromMaybe, isJust)
import           DataStructures.Graphics.DrawTrees
import           Prelude                           hiding (ceiling, floor)
import           Test.QuickCheck

----------------------------------------------------------------------
---  NO MODIFICAR EL CÓDIGO DE ABAJO
----------------------------------------------------------------------

data ABST a = Empty | Node a Int (ABST a) (ABST a) deriving (Show,Eq)

weight Empty            = 0
weight (Node x w lt rt) = w

isABST :: (Ord a) => ABST a -> Bool
isABST Empty          =  True
isABST (Node x w lt rt) =   w == 1 + weight lt + weight rt
                         && keyOf lt  (<x)  && keyOf rt (>x)
                         && isABST lt && isABST rt
  where
    keyOf :: ABST a -> (a -> Bool) -> Bool
    keyOf Empty            p =  True
    keyOf (Node y _ lt rt) p =  p y

empty :: ABST a
empty =  Empty

isEmpty :: ABST a -> Bool
isEmpty Empty =  True
isEmpty _     =  False

---------------------------------------------------
--- ABST generation
---------------------------------------------------

mkABST :: (Ord a) => [a] -> ABST a
mkABST xs = foldl (flip insert) empty xs

{-
*DataStructures.SearchTree.AugmentedBST> pretty $ mkABST "pbdfelasqt"
            ____________('p',10)
           /                    \
    ('b',6)                     ('s',3)
      / \                         / \
('a',1) ('d',4)_            ('q',1) ('t',1)
                \
                ('f',3)
                  / \
            ('e',1) ('l',1)
-}
type T = Char
--  type T = Int
testMkABST = quickCheck (isABST . mkABST :: [T] -> Bool)

-- Generación de un árbol de altura mínima. Éste será un ABST si la lista argumento está
-- ordenada y no tiene repeticiones
minABST:: [a] -> ABST a
minABST xs = fst (aux (length xs) xs)
  where
    aux :: Int -> [a] -> (ABST a, [a])
    aux 0 xs = (Empty, xs)
    aux 1 xs = (Node (head xs) 1 Empty Empty, tail xs)
    aux n xs = (Node y (1+weight t1+ weight t2) t1 t2, zs)
      where
        m = div n 2
        (t1, y:ys) = aux m xs
        (t2, zs) = aux (n-m-1) ys
{-
*DataStructures.SearchTree.AugmentedBST> pretty $ minABST "abcdefghij"
                ____('f',10)____
               /                \
        ('c',5)_                ('i',4)
           /     \                 / \
    ('b',2)     ('e',2)     ('h',2) ('j',1)
      /           /           /
('a',1)     ('d',1)     ('g',1)
-}
testMinABST = quickCheck (isABST . minABST. sort . nub :: [T] -> Bool)

-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

isElem :: (Ord a) => a -> ABST a -> Bool
isElem x t = isJust (search x t)

search :: (Ord a) => a -> ABST a -> Maybe a
search x' Empty  = Nothing
search x' (Node x w lt rt)
  | x'<x         = search x' lt
  | x'>x         = search x' rt
  | otherwise    = Just x

minim :: ABST a -> a
minim Empty               = error "minim on empty tree"
minim (Node x _ Empty rt) = x
minim (Node x _ lt rt)    = minim lt

maxim :: ABST a -> a
maxim Empty               = error "maxim on empty tree"
maxim (Node x _ lt Empty) = x
maxim (Node x _ lt rt)    = maxim rt

-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

-- the function /node/ will be interesting to define delete and insert
node :: a -> ABST a -> ABST a -> ABST a
node x lt rt = Node x (1 + weight lt + weight rt) lt rt

insert :: (Ord a) => a -> ABST a -> ABST a
insert x' Empty  =  Node x' 1 Empty Empty
insert x' (Node x w lt rt)
    | x'<x       =  node x (insert x' lt) rt
    | x'>x       =  node x lt  (insert x' rt)
    | otherwise  =  Node x' w lt rt

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

deleteMin :: (Ord a) => ABST a -> ABST a
deleteMin Empty               =  error "deleteMin sobre árbol vacío"
deleteMin (Node x w Empty rt) = rt
deleteMin (Node x w lt    rt) = Node x (w-1) (deleteMin lt) rt

pDeleteMin t = not(isEmpty t) ==> not(isElem m t') && isABST t'
   where
        m  = minim t
        t' = deleteMin t

testDeleteMin = quickCheck (pDeleteMin :: ABST T -> Property)

delete :: (Ord a) => a -> ABST a -> ABST a
delete x' Empty  =  Empty
delete x' (Node x w lt rt)
  | x'<x       =  node x (delete x' lt) rt
  | x'>x       =  node x lt (delete x' rt)
  | otherwise  =  combine lt rt

combine :: ABST a -> ABST a -> ABST a
combine Empty rt     = rt
combine lt    Empty  = lt
combine lt    rt     = node x' lt rt'
  where (x',rt') = split rt

-- removes and returns minimum element from tree
split :: ABST a -> (a, ABST a)
split (Node x _ Empty rt)  = (x,rt)
split (Node x _ lt rt)  = (x', node x lt' rt)
  where (x',lt') = split lt

{-
*DataStructures.SearchTree.AugmentedBST> pretty $ mkABST "dbmcnfta"
            ('d',8)_
           /        \
    ('b',3)         ('m',4)
      / \             / \
('a',1) ('c',1) ('f',1) ('n',2)
                            \
                            ('t',1)

*DataStructures.SearchTree.AugmentedBST> pretty $ delete 'd' $ mkABST "dbmcnfta"
            ('f',7)
           /    \
    ('b',3)     ('m',3)
      / \           \
('a',1) ('c',1)     ('n',2)
                        \
                        ('t',1)

-}

pDelete x t = not(isElem x t') && isABST t'
   where t' = delete x t

testDelete = quickCheck (pDelete :: T -> ABST T -> Bool)

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

inOrder :: ABST a -> [a]
inOrder t = aux t []
  where
    aux Empty          xs   = xs
    aux (Node x _ lt rt) xs = aux lt (x : aux rt xs)

preOrder :: ABST a -> [a]
preOrder t  = aux t []
  where
    aux Empty          xs   = xs
    aux (Node x _ lt rt) xs = x : aux lt (aux rt xs)

postOrder :: ABST a -> [a]
postOrder t  = aux t []
  where
    aux Empty          xs   = xs
    aux (Node x _ lt rt) xs = aux lt (aux rt (x:xs))

-------------------------------------------------------------------------------
-- Generating arbirtray Binary Search Trees
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (ABST a) where
  arbitrary = do
    xs <- arbitrary
    return (mkABST xs)

-------------------------------------------------------------------------------
-- Drawing a ABST
-------------------------------------------------------------------------------

instance Subtrees (ABST a) where
  subtrees Empty            = []
  subtrees (Node x w lt rt) = [lt,rt]

  isEmptyTree Empty = True
  isEmptyTree  _    = False

instance (Show a) => ShowNode (ABST a) where
  showNode (Node x w lt rt)  = show (x,w)

drawOnWith :: FilePath -> ((a,Int) -> String) -> ABST a -> IO ()
drawOnWith file toString = _drawOnWith file showBST
 where
  showBST (Node x w _ _) = toString (x,w)

-------------------------------------------------------------------------------
-- Pretty Printing a ABST
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

pretty :: (Show a) => ABST a -> IO ()
pretty t = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint t

pprint Empty                =  ([], 0, 0, 0)
pprint (Node x we Empty Empty) =  ([s], ls, 0, ls-1)
  where
    s = show (x,we)
    ls = length s
pprint (Node x we lt rt)         =  (resultLines, w, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show (x,we)
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,_,lc) = pprint lt
    (rp,rw,rc,_) = pprint rt
    -- recurse
    (lw',lb) = if lw==0 then (1," ") else (lw,"/")
    (rw',rb) = if rw==0 then (1," ") else (rw,"\\")
    -- compute full width of this tree
    totLW = maximum [lw', swl,  1]
    totRW = maximum [rw', swr, 1]
    w = totLW + 1 + totRW
{-
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totLW
                -----------    totRW
     ----   -   -----------    w (total width)
-}
    -- get right column info that accounts for left side
    rc2 = totLW + 1 + rc
    -- make left and right tree same height
    llp = length lp
    lrp = length rp
    lp' = if llp < lrp then lp ++ replicate (lrp - llp) "" else lp
    rp' = if lrp < llp then rp ++ replicate (llp - lrp) "" else rp
    -- widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
    lp'' = map (\s -> if length s < totLW then nSpaces (totLW - length s) ++ s else s) lp'
    rp'' = map (\s -> if length s < totRW then s ++ nSpaces (totRW - length s) else s) rp'
    -- first part of line1
    line1 = if swl < lw' - lc - 1 then
                nSpaces (lc + 1) ++ nBars (lw' - lc - swl) ++ s
            else
                nSpaces (totLW - swl) ++ s
    -- line1 right bars
    lline1 = length line1
    line1' = if rc2 > lline1 then
                line1 ++ nBars (rc2 - lline1)
             else
                line1
    -- line1 right padding
    line1'' = line1' ++ nSpaces (w - length line1')
    -- first part of line2
    line2 = nSpaces (totLW - lw' + lc) ++ lb
    -- pad rest of left half
    line2' = line2 ++ nSpaces (totLW - length line2)
    -- add right content
    line2'' = line2' ++ " " ++ nSpaces rc ++ rb
    -- add right padding
    line2''' = line2'' ++ nSpaces (w - length line2'')
    resultLines = line1'' : line2''' : zipWith (\lt rt -> lt ++ " " ++ rt) lp'' rp''

----------------------------------------------------------------------
---  NO MODIFICAR EL CÓDIGO DE ARRIBA
----------------------------------------------------------------------

-- Completa las definiciones de las siguientes funciones sobre árboles binarios
-- aumentados:
--
--    - select
--    - floor
--    - rank
--    - size
--    - partition
--
-- son las mismas que aparecen en el enunciado Java y deben funcionar de forma
-- similar, excepto la última función, 'partition', cuyo enunciado aparece más
-- abajo.
--
-- Para probar tus soluciones puedes utilizar los árboles binarios aumentados
-- 'ejemplo' y 'alfabeto', definidos como sigue:

ejemplo :: ABST Integer
ejemplo = minABST [5, 10 .. 35]

alfabeto :: ABST Char
alfabeto = minABST ['a' .. 'z']

-- para pintar estos árboles puedes usar la función 'pretty':

{-

*DataStructures.SearchTree.AugmentedBST> pretty ejemplo
         (20,7)_
        /       \
  (10,3)        (30,3)
    / \           / \
(5,1) (15,1) (25,1) (35,1)

*DataStructures.SearchTree.AugmentedBST> pretty alfabeto
                                ____________________('n',26)___________________
                               /                                               \
                    ____('g',13)________                                    ____('u',12)____
                   /                    \                                  /                \
            ('d',6)_                    ('k',6)_                    ('r',6)_                ('x',5)_
           /        \                  /        \                  /        \                  /     \
    ('b',3)         ('f',2)     ('i',3)         ('m',2)     ('p',3)         ('t',2)     ('w',2)     ('z',2)
      / \             /           / \             /           / \             /           /           /
('a',1) ('c',1) ('e',1)     ('h',1) ('j',1) ('l',1)     ('o',1) ('q',1) ('s',1)     ('v',1)     ('y',1)


-}

----------------
-- select
----------------

-- Returns the i-th smallest key in ABST (i=0 means returning the smallest value
-- in tree, i=1 the next one and so on). For example:
--
-- *DataStructures.SearchTree.AugmentedBST> select 0 alfabeto
-- Just 'a'
--
-- *DataStructures.SearchTree.AugmentedBST> select 5 alfabeto
-- Just 'f'
--
-- *DataStructures.SearchTree.AugmentedBST> select 60 alfabeto
-- Nothing

select :: Ord a => Int -> ABST a -> Maybe a
select i Empty = Nothing 
select i (Node x w lt rt)
  | i > w = Nothing 
  | i == pesoLT = Just x
  | i < pesoLT = select i lt
  | i > pesoLT = select (i-pesoLT-1) rt
    where pesoLT = weight lt

----------------
-- floor
----------------

-- Returns largest key in ABST whose value is less than or equal to k.
-- For example:
--
-- *DataStructures.SearchTree.AugmentedBST> floor 4 ejemplo
-- Nothing
--
-- *DataStructures.SearchTree.AugmentedBST> floor 5 ejemplo
-- Just 5
--
-- *DataStructures.SearchTree.AugmentedBST> floor 17 ejemplo
-- Just 15
--
-- *DataStructures.SearchTree.AugmentedBST> floor 36 ejemplo
-- Just 35

floor :: Ord a => a -> ABST a -> Maybe a
floor _ Empty = Nothing 
floor k (Node x w lt rt)
  | k < minim (Node x w lt rt) = Nothing 
  | k == x || w == 1 = Just x
  | k < x = floor k lt
  | otherwise = floor k rt

----------------
-- rank
----------------

-- Returns number of keys in ABST whose values are less than x.
-- For example:
--
-- *DataStructures.SearchTree.AugmentedBST> rank 'a' alfabeto
-- 0
--
-- *DataStructures.SearchTree.AugmentedBST> rank 'n' alfabeto
-- 13
--
-- *DataStructures.SearchTree.AugmentedBST> rank 'z' alfabeto
-- 25
--
-- *DataStructures.SearchTree.AugmentedBST> rank 17 ejemplo
-- 3
--
-- *DataStructures.SearchTree.AugmentedBST> rank 37 ejemplo
-- 7
--
-- *DataStructures.SearchTree.AugmentedBST> rank 27 ejemplo
-- 5

rank :: Ord a => a -> ABST a -> Int
rank _ Empty = 0
rank k (Node x w lt rt)
  | x < k = 1 + rank k lt + rank k rt
  | otherwise = rank k lt + rank k rt

----------------
-- size
----------------

-- Returns number of keys in ABST whose values are in range [low, high].
-- For example:
--
-- *DataStructures.SearchTree.AugmentedBST> size 0 12 ejemplo
-- 2
--
-- *DataStructures.SearchTree.AugmentedBST> size 0 15 ejemplo
-- 3
--
-- *DataStructures.SearchTree.AugmentedBST> size 10 20  ejemplo
-- 3
--
-- *DataStructures.SearchTree.AugmentedBST> size 10 30  ejemplo
-- 5

size :: Ord a => a -> a -> ABST a -> Int
size _ _ Empty = 0
size low high (Node x w lt rt)
  | x >= low && x <= high = 1 + size low high lt + size low high rt
  | otherwise = size low high lt + size low high rt

----------------
-- partition
----------------

-- Returns a tuple with two ABST, the first contains keys smaller
-- than a given key, and the second contains keys larger than the
-- given key. For example:
--
-- *DataStructures.SearchTree.AugmentedBST> pretty $ fst $ partition 20 ejemplo
--   (10,3)
--     / \
-- (5,1) (15,1)
--
-- *DataStructures.SearchTree.AugmentedBST> pretty $ snd $ partition 20 ejemplo
--    (30,3)
--      / \
-- (25,1) (35,1)
--
-- *DataStructures.SearchTree.AugmentedBST> pretty $ fst $ partition 30 ejemplo
--          (20,7)
--         /    \
--   (10,3)     (25,1)
--     / \
-- (5,1) (15,1)
--
-- *DataStructures.SearchTree.AugmentedBST> pretty $ snd $ partition 30 ejemplo
-- (35,1)
--
-- *DataStructures.SearchTree.AugmentedBST> pretty $ fst $ partition 'm' alfabeto
--                     ____('g',13)________
--                    /                    \
--             ('d',6)_                    ('k',6)
--            /        \                  /    \
--     ('b',3)         ('f',2)     ('i',3)     ('l',1)
--       / \             /           / \
-- ('a',1) ('c',1) ('e',1)     ('h',1) ('j',1)
--
-- *DataStructures.SearchTree.AugmentedBST> pretty $ snd $ partition 'm' alfabeto
-- ('n',13)____________________
--                             \
--                          ____('u',12)____
--                         /                \
--                  ('r',6)_                ('x',5)_
--                 /        \                  /     \
--          ('p',3)         ('t',2)     ('w',2)     ('z',2)
--            / \             /           /           /
--      ('o',1) ('q',1) ('s',1)     ('v',1)     ('y',1)

partition :: Ord a => a -> ABST a -> (ABST a, ABST a)
partition _ Empty = (Empty, Empty)
partition a (Node x w lt rt)
  | a == x = (lt, rt)
  | a > x = (Node x w lt (fst (particionRT)), snd (particionRT))
  | a < x = (fst (particionLT), Node x w (snd (particionLT)) rt)
    where particionRT = partition a rt
          particionLT = partition a lt