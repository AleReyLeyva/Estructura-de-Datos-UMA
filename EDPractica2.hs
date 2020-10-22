-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 2
--
-- Alumno: Rey Leyva, Alejandro
-------------------------------------------------------------------------------

module Practica2 where

import Data.List
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Ejercicio 2 - máximoYResto
-------------------------------------------------------------------------------
maximo :: Ord a => [a] -> a
maximo (x : xs)
  | length (x : xs) == 0 = error "La lista no puede ser vacía"
  | length (x : xs) == 1 = x
  | otherwise = if (x > head xs) then (maximo (x : (tail xs))) else (maximo xs)

máximoYRestoOrden :: Ord a => [a] -> (a, [a])
máximoYRestoOrden xs = (maximo xs, filter (/= (maximo xs)) xs)

-------------------------------------------------------------------------------
-- Ejercicio 3 - reparte
-------------------------------------------------------------------------------

reparte :: [a] -> ([a], [a])
reparte xs = (impares, pares)
  where
    pares = [x | (x, y) <- (zip xs [1 .. (length xs)]), even y]
    impares = [x | (x, y) <- (zip xs [1 .. (length xs)]), not (even y)]

-------------------------------------------------------------------------------
-- Ejercicio 4 - distintos
-------------------------------------------------------------------------------

distintos :: Eq a => [a] -> Bool
distintos xs
  | length xs <= 1 = True
  | otherwise = (length (filter (== (head xs)) (tail xs)) == 0) && (distintos (tail xs))

-------------------------------------------------------------------------------
-- Ejercicio 6 - divisores
-------------------------------------------------------------------------------

divideA :: Integer -> Integer -> Bool
x `divideA` y = mod y x == 0

divisores :: Integer -> [Integer]
divisores x = [a | a <- [1 .. x], a `divideA` x]

divisores' :: Integer -> [Integer]
divisores' x = [a | a <- [(- x) .. (x)], a `divideA` x]

-------------------------------------------------------------------------------
-- Ejercicio 8 - primos
-------------------------------------------------------------------------------

-- 8.a
esPrimo :: Integer -> Bool
esPrimo x = length (divisores x) == 2

-- 8.b
primosHasta :: Integer -> [Integer]
primosHasta x = [a | a <- [1 .. x], esPrimo a]

-- 8.c
primosHasta' :: Integer -> [Integer]
primosHasta' x = filter (esPrimo) [1 .. x]

-- 8.d
p1_primos :: Integer -> Bool
p1_primos n = primosHasta n == primosHasta' n

-------------------------------------------------------------------------------
-- Ejercicio 13 - hoogle
-------------------------------------------------------------------------------

-- Hoogle (https://www.haskell.org/hoogle/) es un buscador para el API de Haskell.
-- Usa Hoogle para encontrar información sobre las funciones 'and', y 'zip'

desconocida :: Ord a => [a] -> Bool
desconocida xs = and [x <= y | (x, y) <- zip xs (tail xs)]

-------------------------------------------------------------------------------
-- Ejercicio 14 - inserta y ordena
-------------------------------------------------------------------------------

-- 14.a - usando takeWhile y dropWhile
inserta :: (Ord a) => a -> [a] -> [a]
inserta y xs = (takeWhile (< y) xs) ++ (y : []) ++ (dropWhile (< y) xs)

-- 14.b - mediante recursividad
insertaRec :: (Ord a) => a -> [a] -> [a]
insertaRec y xs = if (y <= (head xs)) then y : xs else (head xs) : (insertaRec y (tail xs))

-- 14.c

-- La línea de abajo no compila hasta que completes los apartados
-- anteriores.

p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

-- 14.e - usando foldr
ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr (inserta) [] xs

-- 14.f
prop_ordena_OK xs = ordenado (ordena xs)
  where
    ordenado xs
      | length xs <= 1 = True
      | length xs == 2 = head xs <= (head (tail xs))
      | otherwise = (head xs <= (head (tail xs))) && (ordenado (tail xs))

-------------------------------------------------------------------------------
-- Ejercicio 21 - nub
-------------------------------------------------------------------------------

-- 21.a
nub' :: Eq a => [a] -> [a]
nub' xs
  | length xs <= 1 = xs
  | otherwise = head xs : (nub' (filter (/= (head xs)) (tail xs)))

-- 21.b
p_nub' xs = nub xs == nub' xs

-- 21.c (distintos se define en el ejercicio 2)

p_sinRepes xs = distintos (nub' xs)

{-

Escribe aquí tu razonamiento de por qué p_sinRepes no
es suficiente para comprobar que nub' es correcta:
  Que todos los elementos sean distintos entre si no implica que
  se haya llevado a cabo una correcta eliminación de los repetidos,
  es decir, es probable que se haya eliminado totalmente un número
  que originalmente estaba en la lista inicial.

-}

-- 21.d
todosEn :: Eq a => [a] -> [a] -> Bool
ys `todosEn` xs = all (`elem` xs) ys

p_sinReps' xs = p_sinRepes xs ==> (nub' xs) `todosEn` xs
