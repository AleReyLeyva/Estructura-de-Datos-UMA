------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo cliente del TAD Stack
------------------------------------------------------------

module StackClient where

import           Stack
-- import DataStructures.Stack.LinearStack
-- import DataStructures.Stack.StackOnList


pila1 :: Stack Int
pila1 = push 3 (push 4 push 5 push 7 empty)
















-- Si Stack es un verdadero TAD, no tengo acceso a la implementación.

-- No conozco los contructores Node ni Empty

-- El CLIENTE del TAD: solo puedo utilizar las operaciones
-- públicas (empty, push, pop, top, isEmpty)


-- En Java usamos atributos private y protected.
-- ¿Cómo controlo la privacidad en Haskell?


-- El cliente no conoce la representación física.
-- Utiliza la pila exclusivamente a través de la interfaz.
-- Todas las implementaciones tienen la misma interfaz y propiedades.


pila2 :: Stack Char
pila2 = push 'a' (push 'b' (push 'c' (push 'd' empty)))

pila3 :: Stack (String, Int)
pila3 = push ("neon", 10) (push ("hidrogeno", 1) (push ("oxigeno", 8) empty))

-- calcula el número de elementos de una pila
size :: Stack a -> Int
size s
   | isEmpty s = 0
   | otherwise = 1 + size (pop s)

-- convierte una lista en una pila
list2Stack :: [a] -> Stack a
list2Stack xs = foldr push empty xs
