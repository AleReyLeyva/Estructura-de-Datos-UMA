
{- |

Estructuras de Datos
2.º A Computadores, 2.º D Informática, 2.º Matemáticas + Informática

Práctica Evaluable - noviembre 2020

Apellidos, Nombre:

Grupo:

-}

module SetMultiMapClient where

import qualified DataStructures.Set.LinearSet as S
import           SetMultiMap

mkSet :: Eq a => [a] -> S.Set a
mkSet = foldr S.insert S.empty

mxs :: SetMultiMap String Int
mxs = insert "malaga"  1 (
      insert "malaga"  3 (
      insert "malaga"  5 (
      insert "granada" 7 (
      insert "granada" 5 (
      insert "granada" 9
      empty)))))

mys :: SetMultiMap Int Char
mys = insert 1 'a' (
      insert 1 'b' (
      insert 2 'c' (
      insert 5 'd' (
      insert 5 'e' (
      insert 9 'f'
      empty)))))

-- | Ejercicio 3 - Uso del TAD
----------------------------------------------------------------------

-- 1,25 ptos.
-- |
-- >>> compose mxs mys
-- "granada" --> LinearSet('d','e','f')
-- "malaga" --> LinearSet('a','b','d','e')

compose :: (Ord a, Ord b, Eq c) => SetMultiMap a b -> SetMultiMap b c -> SetMultiMap a c
compose xs ys = fold f empty xs
  where
    f k v restoMap = fold g restoMap ys
      where g q w restoMap2 = if (q == v) then insert k w restoMap2 else restoMap2


