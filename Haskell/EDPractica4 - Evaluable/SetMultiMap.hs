{- |

Estructuras de Datos
2.º A Computadores, 2.º D Informática, 2.º Matemáticas + Informática

Práctica Evaluable - noviembre 2020

Apellidos, Nombre: Rey Leyva, Alejandro

Grupo: 2.º Matemáticas + Informática

-}

module SetMultiMap( SetMultiMap
                  , empty
                  , isEmpty
                  , size
                  , isDefinedAt
                  , insert
                  , deleteKey
                  , deleteKeyValue
                  , valuesOf
                  , filterValues
                  , fold
                  ) where

import           Data.List                    (intercalate)
import           Test.QuickCheck

import qualified DataStructures.Set.LinearSet as S

-- Invariante de representación:
--   - Los nodos están ordenados por clave
--   - No hay claves repetidas
--   - No hay claves que tengan asociado un conjunto vacío

data SetMultiMap a b = Empty
                     | Node a (S.Set b) (SetMultiMap a b)
                     deriving Eq

-- ejemplo de SetMultiMap para probar las funciones

m1 :: SetMultiMap String Int
m1 = Node "alfredo" (mkSet [9]) (
     Node "juan"    (mkSet [0,1,8]) (
     Node "maria"   (mkSet [4,-6,8])
     Empty))

mkSet :: Eq a => [a] -> S.Set a
mkSet = foldr S.insert S.empty


-- | Ejercicio 1 - Definición de operaciones y complejidad
----------------------------------------------------------------------

-- 0,25 ptos.
-- |
-- >>> empty
-- {}

-- Complejidad: O(1)
empty :: SetMultiMap a b
empty = Empty

-- 0,25 ptos.
-- |
-- >>> isEmpty m1
-- False

-- Complejidad: O(1)
isEmpty :: SetMultiMap a b -> Bool
isEmpty Empty = True
isEmpty (Node x xs s) = False

-- 1 pto.
-- |
-- >>> size m1
-- 3

-- Complejidad: O(n)
size :: SetMultiMap a b -> Integer
size Empty = 0
size (Node x xs s) = 1 + (size s)

-- 1 pto.
-- |
-- >>> isDefinedAt "maria" m1
-- True
--
-- >>> isDefinedAt "eva" m1
-- False

-- Complejidad: O(n)
isDefinedAt :: (Ord a, Eq a) => a -> SetMultiMap a b -> Bool
isDefinedAt k Empty = False
isDefinedAt k (Node x xs s)
  | k < x = False
  | otherwise = (x == k) || (isDefinedAt k s)

-- 1 pto.
-- |
-- >>> insert "alfredo" 5 m1
-- "alfredo" --> LinearSet(9,5)
-- "juan" --> LinearSet(8,1,0)
-- "maria" --> LinearSet(8,-6,4)
--
-- >>> insert "carmen" 20 m1
-- "alfredo" --> LinearSet(9)
-- "carmen" --> LinearSet(20)
-- "juan" --> LinearSet(8,1,0)
-- "maria" --> LinearSet(8,-6,4)

-- Complejidad: O(n)
insert :: (Ord a, Eq b) => a -> b -> SetMultiMap a b -> SetMultiMap a b
insert k v Empty = Node k (mkSet [v]) Empty
insert k v (Node x xs s)
  | k < x = Node k (mkSet [v]) (Node x xs s)
  | k == x = Node x (S.insert v xs) s
  | otherwise = Node x xs (insert k v s)

-- 1 pto.
-- |
-- >>> deleteKey "juan" m1
-- "alfredo" --> LinearSet(9)
-- "maria" --> LinearSet(8,-6,4)

-- Complejidad: O(n)
deleteKey :: (Ord a, Eq b) => a -> SetMultiMap a b -> SetMultiMap a b
deleteKey k Empty = Empty
deleteKey k (Node x xs s)
  | k < x = (Node x xs s)
  | k == x = s
  | otherwise = Node x xs (deleteKey k s)

-- 1 pto.
-- |
-- >>> deleteKeyValue "maria" 4 m1
-- "alfredo" --> LinearSet(9)
-- "juan" --> LinearSet(8,1,0)
-- "maria" --> LinearSet(8,-6)

-- Complejidad: O(n)
deleteKeyValue :: (Ord a, Eq b) => a -> b -> SetMultiMap a b -> SetMultiMap a b
deleteKeyValue k v Empty = Empty
deleteKeyValue k v (Node x xs s)
  | k < x = (Node x xs s)
  | k == x = if (S.isEmpty (postSet)) then s else Node x (postSet) s
  | otherwise = Node x xs (deleteKeyValue k v s)
      where postSet = S.delete v xs

-- 1 pto.
-- |
-- >>> valuesOf "maria" m1
-- Just LinearSet(8,-6,4)
--
-- >>> valuesOf "paco" m1
-- Nothing

-- Complejidad: O(n)
valuesOf :: (Ord a, Eq b) => a -> SetMultiMap a b -> Maybe (S.Set b)
valuesOf k Empty = Nothing
valuesOf k (Node x xs s)
  | k < x = Nothing
  | k == x = Just xs
  | otherwise = valuesOf k s

-- 1,25 ptos.
-- |
-- >>> filterValues (> 0) m1
-- "alfredo" --> LinearSet(9)
-- "juan" --> LinearSet(8,1)
-- "maria" --> LinearSet(8,4)

-- Complejidad: O()
filterValues :: (Ord a, Eq b) => (b -> Bool)-> SetMultiMap a b -> SetMultiMap a b
filterValues p Empty = Empty
filterValues p (Node x xs s)
  | S.isEmpty postSet = filterValues p s
  | otherwise = Node x postSet (filterValues p s)
       where
         f x resto = if p x then S.insert x resto else resto
         postSet = S.fold f (mkSet []) xs

-- | Ejercicio 2 - Axiomas del TAD
----------------------------------------------------------------------

-- 1 pto.
-- | completa los axiomas que definen deleteKeyValue

-- deleteKeyValue :: (Ord a, Eq b) => a -> b -> SetMultiMap a b -> SetMultiMap a b
ax_deleteKV_empty k v = deleteKeyValue k v empty == empty
ax_deleteKV_insert_1 k v s = deleteKeyValue k v (insert k v s) == deleteKeyValue k v s



------------------------------ NO EDITAR EL CÓDIGO DE ABAJO ------------------------------

fold :: (Ord a, Eq b) => (a -> b -> c -> c) -> c -> SetMultiMap a b -> c
fold f z ms = recSetMultiMap ms
  where
    recSetMultiMap Empty = z
    recSetMultiMap (Node k s ms)
      | S.isEmpty s = recSetMultiMap ms
      | otherwise = f k v (recSetMultiMap (Node k s' ms))
      where
        (v, s') = pickOne s
    pickOne s = (v, S.delete v s)
      where v = head $ S.fold (:) [] s

instance (Show a, Show b) => Show(SetMultiMap a b) where
  show Empty         = "{}"
  show ms            = intercalate "\n" (showKeyValues ms)
    where
      showKeyValues Empty = []
      showKeyValues (Node k s ms) = (show k ++ " --> " ++ show s) : showKeyValues ms

instance  (Ord a, Arbitrary a, Eq b, Arbitrary b) => Arbitrary (SetMultiMap a b) where
    arbitrary = do
      xs <- listOf arbitrary
      ys <- listOf arbitrary
      return (foldr (uncurry insert) empty (zip xs ys))
