-------------------------------------------------------------------
-- Estructuras de Datos
-- Grados en Ingeniería Informática, del Software y de Computadores
-- Tema 2. Características de la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema2 where

import           Data.Char
import           Data.List
import           Test.QuickCheck

{-

Listas

Una lista se escribe como una secuenia de expresiones separadas
por comas y encerradas entre corchetes:

      [e1, e2, e3, ..., en]

Por ejemplo:

      [1,2,3,4,5]
      [7, 1, -6]
      ['a', 'b', 'c']
      [1 < 2]
      []

El tipo de una lista se escribe entre corchetes:

      ['a', 'b', 'c'] :: [Char]
      [1 + 1 == 2, 'a' < 'b'] :: [Bool]
      [True] :: [Bool]

Diferencia con las tuplas:

      1. todos los elementos deben tener el mismo tipo
      2. la longitud de la lista puede variar, pero el tipo es el mismo

Las listas son instancia de las clases Eq y Ord siempre que el
tipo de los elementos sea instancia de Eq y Ord.

      [1,1+1,3] == [1,2,3]
      [5, 7, 9] < [5, 7, 3]

Las listas de tipo base Char se pueden escribir como cadenas:

      ['a', 'b', 'c'] es lo mismo que "abc"

Al tipo [Char] también se le llama String:

      "abc" :: String
-}


{-

Ejercicio: tipar las siguientes expresiones


   [1, 2, 3]

   "1,2,3"

   [ ('a', 8), ('t', 6) ]

   [ (True) ]

   ( [True] )

   [ [], [2, 3] ]

   [ ["a"] ]

   [ ['a'] ]

   [ () ]

   ( [] )

   [ 1 == 1 + 1, -3 < 3 ]

   [ 1 == 1 + 1, undefined, -3 < 3 ]

   [ error "algo va mal" ]

-}

-- funciones predefinidas en Prelude

{-

   null

   head

   last

   tail

   init

   take

   drop


    head              tail
     |   ------------------------------
    [x1, x2, x3, ............, xn-1, xn]
     ------------------------------  |
                 init               last

    -------------- + -----------------
         take              drop

   elem / notElem
   length
   ++ (concatenación de listas)
-}

-- QuickCheck: propiedades de la concatenación

prop_identidad :: Eq a => [a] -> Bool
prop_identidad xs =
   xs ++ [] == xs
         &&
   [] ++ xs == xs

prop_asociativa :: Eq a => [a] -> [a] -> [a] -> Bool
prop_asociativa xs ys zs =
   (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- cuidado: QuickCheck utiliza por defecto el tipo ()
-- para comprobar las propiedades polimórficas
prop_disparate :: Eq a => a -> [a] -> Bool
prop_disparate x xs =
   x : xs == xs ++ [x]

-- constructores de listas: [] y :

--  [x1, x2, x3 ] = x1 : x2 : x3 : []

-- patrones de listas


-- listas y recursividad: patrones y definición por casos

-- |
-- >>> longitud [1,2,3]
-- 3
--
-- >>> longitud []
-- 0
--
-- >>> longitud "haskell"
-- 7

longitud :: [a] -> Integer -- predefinida como length
longitud xs =  undefined

-- |
-- >>> ordenada [-6, 4, 17]
-- True
--
-- >>> ordenada "java"
-- False

ordenada :: Ord a => [a] -> Bool
ordenada xs = undefined

-- |
-- >>> tresIguales [1,1,1]
-- True
--
-- tresIguales [5, 2+3, 8-3]
-- True
--
-- >>> tresIguales "ala"
-- False

tresIguales ::  [Integer] -> Bool
tresIguales xs = undefined

-- |
-- tresDoses [2,1+1,2]
-- True
--
-- >>> tresDoses [1+1, 3-1, 5-1]
-- False

tresDoses :: [Integer] -> Bool
tresDoses xs = undefined

-- eficiencia: conteo de reducciones

-- eficiencia de length

-- eficiencia de ++

-- eficiencia de inversa

-- |
-- >>> inversa "abc"
-- "cba"

inversa :: [a] -> [a] -- predefinida como reverse
inversa xs = undefined
