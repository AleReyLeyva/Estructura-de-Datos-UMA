-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 3 - Uso del TAD Bag mediante plegado
--
-- Alumno: Rey Leyva, Alejandro
-------------------------------------------------------------------------------

module BagClient where

import           LinearBag

-------------------------------------------------------------------------------
-- convertir una lista en una bolsa
-- |
-- >>> mkBag "abracadabra"
-- LinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }

mkBag :: Ord a => [a] -> Bag a
mkBag xs = foldr insert empty xs

-------------------------------------------------------------------------------
-- Uso del TAD Bag a través del plegado
-------------------------------------------------------------------------------

-- El TAD Bag es casi inútil sin una función de plegado. El plegado es un
-- iterador que permite recorrer una bolsa sin conocer su implementación.

-------------------------------------------------------------------------------
-- keys: devuelve una lista con las claves que aparecen en la bolsa
-- |
-- >>> keys (mkBag "abracadabra")
-- "abcdr"

keys :: Ord a => Bag a -> [a]
keys bag = foldBag f [] bag
   where
      f x ox solRestoBolsa = x : solRestoBolsa

-------------------------------------------------------------------------------
-- Ejercicios de plegados
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  bag2list: convierte una bolsa en una lista de pares (a,Int)
-- |
-- >>> bag2List (mkBag "abracadabra")
-- [('a',5),('b',2),('c',1),('d',1),('r',2)]

bag2List :: Ord a => Bag a -> [(a, Int)]
bag2List s = foldBag f [] s
 where f x ox solRestoBolsa = [(x,ox)] ++ solRestoBolsa
 
-------------------------------------------------------------------------------
-- cardinal: devuelve el número de elementos de una bolsa
-- |
-- >>> cardinal (mkBag "abracadabra")
-- 11

cardinal :: Ord a => Bag a -> Int
cardinal bag = foldBag f 0 bag  
  where f x ox solRestoBolsa = solRestoBolsa + ox

-------------------------------------------------------------------------------
-- contains: devuelve True si el dato aparece en la bolsa, False en caso contrario
-- |
-- >>> contains 'a' (mkBag "abracadabra")
-- True
-- >>> contains 'z' (mkBag "abracadabra")
-- False

contains :: Ord a => a -> Bag a -> Bool
contains x bag = foldBag f False bag
  where f y oy solRestoBolsa = (x==y) || solRestoBolsa 

-------------------------------------------------------------------------------
-- maxOcurrences: devuelve el número de veces que aparece el elemento que
-- aparece más veces en una bolsa
-- |
-- >>> maxOcurrences (mkBag "abracadabra")
-- 5

maxOcurrences :: Ord a => Bag a -> Int
maxOcurrences bag = foldBag f 0 bag 
  where f x ox solRestoBolsa = if (ox > solRestoBolsa) then ox else solRestoBolsa

-------------------------------------------------------------------------------
-- mostCommons: devuelve los elementos que aparecen más veces en la bolsa y su
-- cardinal común
-- |
-- >>> mostCommons (mkBag "abcabcab")
-- [('a',3),('b',3)]

mostCommons :: Ord a => Bag a -> [(a,Int)]
mostCommons bag = foldBag f [] bag
	where f x ox solRestoBolsa = if (ox == maxOcurrences bag) then [(x, ox)] ++ solRestoBolsa else solRestoBolsa
