-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 2 - Ejercicios extra
--
-- Alumno: Rey Leyva, Alek
-------------------------------------------------------------------------------

module Practica2Extra where

import Data.Char
import Test.QuickCheck
import Text.Show.Functions

----------------------------------------------------------------------
-- Ejercicio - empareja
----------------------------------------------------------------------

empareja :: [a] -> [b] -> [(a, b)]
empareja _xs [] = []
empareja [] _ys = []
empareja (x : xs) (y : ys) = (x, y) : (empareja xs ys)

prop_empareja_OK :: (Eq b, Eq a) => [a] -> [b] -> Bool
prop_empareja_OK xs ys = (zip xs ys) == (empareja xs ys)

----------------------------------------------------------------------
-- Ejercicio - emparejaCon
----------------------------------------------------------------------

emparejaCon :: (a -> b -> c) -> [a] -> [b] -> [c]
emparejaCon f _xs [] = []
emparejaCon f [] _ys = []
emparejaCon f (x : xs) (y : ys) = [f x y] ++ emparejaCon f xs ys

prop_emparejaCon_OK :: Eq c => (a -> b -> c) -> [a] -> [b] -> Bool
prop_emparejaCon_OK f xs ys = (zipWith f xs ys) == (emparejaCon f xs ys)

----------------------------------------------------------------------
-- Ejercicio - separa
----------------------------------------------------------------------

separaRec :: (a -> Bool) -> [a] -> ([a], [a])
separaRec p xs = undefined

separaC :: (a -> Bool) -> [a] -> ([a], [a])
separaC p xs = ([x | x <- xs, p x], [x | x <- xs, not (p x)])

separaP :: (a -> Bool) -> [a] -> ([a], [a])
separaP p xs = undefined

prop_separa_OK :: Eq a => (a -> Bool) -> [a] -> Bool
prop_separa_OK p xs = (separaRec p xs == separaC p xs) && (separaC p xs == separaP p xs)

----------------------------------------------------------------------
-- Ejercicio - lista de pares
----------------------------------------------------------------------

cotizacion :: [(String, Double)]
cotizacion = [("apple", 116), ("intel", 35), ("google", 824), ("nvidia", 67)]

buscarRec :: Eq a => a -> [(a, b)] -> [b]
buscarRec x ys
  | length ys == 0 = []
  | length ys == 1 = if (fst (head ys) == x) then [snd (head ys)] else []
  | otherwise = if (fst (head ys) == x) then [snd (head ys)] else buscarRec x (tail ys)

buscarC :: Eq a => a -> [(a, b)] -> [b]
buscarC x ys = [(snd y) | y <- ys, (fst y) == x]

buscarP :: Eq a => a -> [(a, b)] -> [b]
buscarP x ys = undefined

prop_buscar_OK :: (Eq a, Eq b) => a -> [(a, b)] -> Bool
prop_buscar_OK x ys = (buscarRec x ys) == (buscarC x ys) && (buscarC x ys) == (buscarP x ys)

{-

Responde las siguientes preguntas si falla la propiedad anterior.

¿Por qué falla la propiedad prop_buscar_OK?

Realiza las modificaciones necesarias para que se verifique la propiedad.

-}

valorCartera :: [(String, Double)] -> [(String, Double)] -> Double
valorCartera cartera mercado = undefined

----------------------------------------------------------------------
-- Ejercicio - mezcla
----------------------------------------------------------------------

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla xs [] = xs
mezcla [] ys = ys
mezcla [x] ys = x : [a | a <- ys, a >= x]
mezcla (x : xs) (y : ys) = (x : [a | a <- (y : ys), a >= x && a <= head xs]) ++ (mezcla xs (y : ys))

----------------------------------------------------------------------
-- Ejercicio - takeUntil
----------------------------------------------------------------------

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p xs = undefined

prop_takeUntilOK :: Eq a => (a -> Bool) -> [a] -> Bool
prop_takeUntilOK p xs = undefined

----------------------------------------------------------------------
-- Ejercicio - número feliz
----------------------------------------------------------------------
digitosDeAc :: Integer -> Integer -> [Integer]
digitosDeAc x n
  | div x (10 ^ n) == 0 = reverse [mod (div x (10 ^ m)) 10 | m <- [0 .. (n -1)]]
  | otherwise = digitosDeAc x (n + 1)

digitosDe :: Integer -> [Integer]
digitosDe x = digitosDeAc x 1

sumaCuadradosDigitos :: Integer -> Integer
sumaCuadradosDigitos x = foldr (+) 0 (map (\x -> x * x) (digitosDe x))

esFelizAc :: Integer -> Integer -> Bool
esFelizAc 1 n = True
esFelizAc x n = if (length (digitosDe x) == 1 && n > 1) then False else esFelizAc (sumaCuadradosDigitos x) (n + 1)

esFeliz :: Integer -> Bool
esFeliz x = esFelizAc x 1

felicesHasta :: Integer -> [Integer]
felicesHasta n = [x | x <- [1 .. n], esFeliz x]

{-

Responde a la siguiente pregunta.

¿Cuántos números felices hay menores o iguales que 1000? 143

-}

----------------------------------------------------------------------
-- Ejercicio - borrar
----------------------------------------------------------------------

borrarRec :: Eq a => a -> [a] -> [a]
borrarRec x ys = undefined

borrarC :: Eq a => a -> [a] -> [a]
borrarC x ys = undefined

borrarP :: Eq a => a -> [a] -> [a]
borrarP x ys = undefined

prop_borrar_OK :: Eq a => a -> [a] -> Bool
prop_borrar_OK x ys = undefined

----------------------------------------------------------------------
-- Ejercicio - agrupar
----------------------------------------------------------------------

agrupar :: Eq a => [a] -> [[a]]
agrupar xs = undefined

----------------------------------------------------------------------
-- Ejercicio - aplica
----------------------------------------------------------------------

aplicaRec :: a -> [a -> b] -> [b]
aplicaRec x fs = undefined

aplicaC :: a -> [a -> b] -> [b]
aplicaC x fs = undefined

aplicaP :: a -> [a -> b] -> [b]
aplicaP x fs = undefined

aplicaM :: a -> [a -> b] -> [b]
aplicaM x fs = undefined

prop_aplica_OK :: Eq b => a -> [a -> b] -> Bool
prop_aplica_OK x fs = undefined
