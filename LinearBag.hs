-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 3 - Implementación y Especificación del TAD Bag
--
-- Alumno: APELLIDOS, NOMBRE
-------------------------------------------------------------------------------

module LinearBag
  ( Bag          -- :: Bag a
  , empty        -- :: Bag a
  , isEmpty      -- :: Bag a -> Bool
  , insert       -- :: Ord a => a -> Bag a -> Bag a
  , delete       -- :: Ord a => a -> Bag a -> Bag a
  , occurrences  -- :: Ord a => a -> Bag a -> Int
  , union        -- :: Ord a => Bag a -> Bag a -> Bag a
  , intersection -- :: Ord a => Bag a -> Bag a -> Bag a
  , difference   -- :: Ord a => Bag a -> Bag a -> Bag a

  ) where

import           Data.List       (sort, (\\))
import           Test.QuickCheck

-- Estas funciones del módulo Data.List se utilizan en las propiedades
-- QuickCheck. No necesitas usarlas en la implementación.
--
--    sort xs  - devuelve la ordenación de la lista xs
--    xs // ys - devuelve la diferencia entre las listas xs e ys

-------------------------------------------------------------------------------
-- Implementación del TAD Bag
-------------------------------------------------------------------------------

data Bag a = Empty
           | Node a Int (Bag a) -- clave contador
           deriving Eq

-- mantenemos los nodos ordenados por clave
bolsa1 :: Bag Char
bolsa1 = Node 'a' 2 (Node 'b' 3 (Node 'c' 2 (Node 'd'  1 Empty)))

{-

Consideraremos el siguiente INVARIANTE de la representación:

   I1 : la información de los nodos estará ordenada sin duplicidades:

              Nodo x ox (Nodo y oy s) implica que  x < y

   I2 : no deben aparecer nodos con el contador de ocurrencias <= 0

              Nodo x ox s   implica que  ox > 0

Esto permitirá generar una igualdad estructural (deriving Eq).

Todas las operaciones del TAD Bag reciben una bolsa que satisface el
invariante y, si devuelven una bolsa, ésta debe satisfacer el invariante.

-}

-- convertir una lista en una bolsa
mkBag :: Ord a => [a] -> Bag a
mkBag xs = foldr insert empty xs

{-

La función mkBag permite convertir una lista en una bolsa.
Por ejemplo:

   mkBag "abracadabra"

devuelve la bolsa:

   Node 'a' 5 (Node 'b' 2 (Node 'c' 1 (Node 'd' 1 (Node 'r' 2 Empty))))

que se representa mediante show como:

   LinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }

Puedes usar mkBag para construir bolsas para comprobar las
funciones de bolsas. Por ejemplo para probar delete podemos usar:

   > delete 'a' (mkBag "haskell")
   LinearBag { 'e' 'h' 'k' 'l' 'l' 's' }

   > delete 'l' (mkBag "haskell")
   LinearBag { 'a' 'e' 'h' 'k' 'l' 's' }

   > delete 'x' (mkBag "haskell")
   LinearBag { 'a' 'e' 'h' 'k' 'l' 'l' 's' }

   > delete 'a' (mkBag "haskell") == mkBag "hskell"
   True

-}


-- devuelve una bolsa vacía
empty :: Bag a
empty = Empty

-- comprueba si una bolsa está vacía
isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- inserta un nuevo dato en una bolsa
insert :: Ord a => a -> Bag a -> Bag a
insert x Empty         = Node x 1 Empty
insert x (Node y oy s)
  | x < y = Node x 1 (Node y oy s)
  | x == y = Node y (oy+1) s
  | otherwise = Node y oy (insert x s)

-- devuelve el número de ocurrencias de un elemento en una bolsa
-- (0 si el elemento no está en la bolsa)
occurrences :: (Ord a) => a -> Bag a -> Int
occurrences x Empty         = 0
occurrences x (Node y oy s) 
  | x == y = oy
  | otherwise = occurrences x s

-- borra una ocurrencia de un dato de una bolsa
-- (devuelve la bolsa original si el dato no estaba en la bolsa)
delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty         = Empty
delete x (Node y oy s)
  | x == y || x < y = s
  | otherwise = Node y oy (delete x s)

-- instancia de la clase Show para imprimir las bolsas
instance Show a => Show (Bag a) where
   show s = "LinearBag { " ++ show' s
    where
       show' Empty         = "}"
       show' (Node x ox s) = muestra x ox ++  show' s
       muestra x 0  = ""
       muestra x ox = show x ++ ' ' : muestra x (ox-1)

-------------------------------------------------------------------------------
-- Especificación del TAD Bag
-------------------------------------------------------------------------------

-- generación de bolsas aleatorias para QuickCheck
instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
  arbitrary =  do
                  xs <- listOf arbitrary
                  return (foldr insert empty xs)

-- selectores
isEmpty_empty = isEmpty empty == True
isEmpty_insert x s = isEmpty (insert x s) == False

occurrences_empty x = occurrences x empty == 0
occurrences_insert_1 x s = occurrences x (insert x s) == 1 + occurrences x s

occurrences_insert_2 x y s = x /= y ==> occurrences x (insert y s) == occurrences x s

-- transformadores
delete_empty x = delete x empty == empty
delete_insert_1 x s = delete x (insert x s) == res
  where res = if (occurrences x s == 0) then s else (delete x s)
delete_insert_2 x y s  = x /= y ==> delete x (insert y s) == res
  where res = if (occurrences x s == 0) then (insert y s) else (delete x (insert y s))

type T = Char -- Integer, etc.

check_Bag :: IO ()
check_Bag = do
               quickCheck (isEmpty_empty :: Bool)
               quickCheck (isEmpty_insert :: T -> Bag T -> Bool)
               quickCheck (occurrences_empty :: T -> Bool)
               quickCheck (occurrences_insert_1 :: T -> Bag T -> Bool)
               quickCheck (occurrences_insert_2 :: T -> T -> Bag T -> Property)
               quickCheck (delete_empty :: T -> Bool)
               quickCheck (delete_insert_1 :: T -> Bag T -> Bool)
               quickCheck (delete_insert_2 :: T -> T -> Bag T -> Property)

-------------------------------------------------------------------------------
-- Operaciones auxiliares del TAD Bag
-------------------------------------------------------------------------------

{-

Añadir al módulo las siguientes funciones para bolsas:

   unión de bolsas
   intersección de bolsas
   diferencia de bolsas

Estas funciones son semejantes a las de los conjuntos pero teniendo
en cuenta las ocurrencias de cada elemento.

-}

union :: Ord a => Bag a -> Bag a -> Bag a
union s Empty                     = s
union Empty s                     = s
union (Node x ox s) (Node y oy t)
  | ox == 1 = union s (insert x (Node y oy t))
  | otherwise = s

intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection s Empty                     = Empty
intersection Empty s                     = Empty
intersection (Node x ox s) (Node y oy t) 
  | occurrences_x == 0 = intersection s (Node y oy t)
  | occurrences_x /= 0 && ox < occurrences_x = Node x ox (intersection s (Node y oy t))
  | otherwise = Node x occurrences_x (intersection s (Node y oy t))
     where occurrences_x = occurrences x (Node y oy t)

difference :: Ord a => Bag a -> Bag a -> Bag a
difference s Empty                     = s
difference Empty s                     = Empty
difference (Node x ox s) (Node y oy t)
  | occurrences_x == 0 = Node x ox (difference s (Node y oy t))
  | occurrences_x /= 0 && ox <= occurrences_x = difference s (Node y oy t)
  | otherwise = Node x (ox-occurrences_x) (difference s (Node y oy t))
     where occurrences_x = occurrences x (Node y oy t)

-- propiedades QuickCheck para comprobar union, intersection y difference

check_union xs ys =
   union (mkBag xs) (mkBag ys) == mkBag (xs ++ ys)

check_intersection xs ys =
   intersection (mkBag xs) (mkBag ys) == mkBag (intersecta (sort xs) (sort ys))
    where
      intersecta [] _ = []
      intersecta _ [] = []
      intersecta (x:xs) (y:ys)
        | x == y = x : intersecta xs ys
        | x <  y = intersecta xs (y:ys)
        | otherwise = intersecta (x:xs) ys

check_difference xs ys =
   difference (mkBag xs) (mkBag ys) == mkBag (xs \\ ys)

check_bag_aux = do
                   quickCheck (check_union :: [T] -> [T] -> Bool)
                   quickCheck (check_intersection :: [T] -> [T] -> Bool)
                   quickCheck (check_difference :: [T] -> [T] -> Bool)

-------------------------------------------------------------------------------
-- Eficiencia de la implementación del TAD Bag
-------------------------------------------------------------------------------

{-

Responde a las dos siguientes preguntas sobre la eficiencia de nuestra
representación del TAD Bag.

1) Completa la siguiente tabla indicando mediante la notación O el número
de pasos que realiza cada operación del TAD Bag

    operación        números de pasos
    ---------------------------------
    empty                O(1)
    isEmpty
    insert
    delete
    occurrences



2) Nuestra representación del TAD Bag mantiene la secuencia de nodos ordenados:

        Node 'a' 2 (Node 'b' 3 (Node 'c' 2 (Node 'd'  1 Empty)))

Obviamente, también es posible representar el TAD Bag por una secuencia desordenada:

        Node 'c' 2 (Node 'd' 1 (Node 'a' 2 (Node 'b'  3 Empty)))

¿Cuál de las dos representaciones es más eficiente? ¿Por qué?



-}
