-------------------------------------------------------------------------------
-- Data Structures. February 2021.
-- Grado en Informática. UMA.
--
-- Student's name: Alejandro Rey Leyva
-- Student's group: Matemáticas + Informática
-------------------------------------------------------------------------------

module Tren where

-- =====================================================
-- Un tren está compuesto por una máquina y vagones
-- Los vagones pueden transportar objetos identififcados por su peso (Int).
-- Cada vagón puede transportar varios objetos hasta un peso máximo
-- (dado por la función tope)

import           Data.List

data Tren = Maquina | Vagon Int [Int] Tren

-- Maquina: un tren sin vagones
-- Vagon cr xs tren
--    cr es la capacidad restante del vagón (peso que aún cabe en el vagon)
--    xs es la lista de pesos que ya lleva el vagón
--    tren el resto del tren
-- Cuando se crea un vagón, el valor de la capacidad restante es tope y la
-- lista de pesos que ya lleva es vacía

-- Capacidad máxima de un vagón

tope :: Int
tope = 10 -- no se usa, se puede quitar

-- del: borra un objeto de un peso dado del tren. error si ese  peso no está en el tren
-- Encuentra el primer peso que coincida y lo quita del vagon.
-- Si como resultado de eliminar un peso, un vagón queda vacío de objetos
-- se desconecta del tren

-- =====================================================
-- Ejemplos de prueba

ejemplo1 :: Tren
ejemplo1 = Vagon 2 [5,3] (Vagon 3 [3,2,2] (Vagon 5 [5] Maquina))

ejemplo2 :: Tren
ejemplo2 = Vagon 1 [2,1,1,3,2] (Vagon 4 [4,2] Maquina)

-- |
-- >>> del 5 ejemplo1
-- (7,[3])-(3,[3,2,2])-(5,[5])-XxIx>
--
-- >>> del 5 (del 5 ejemplo1)
-- (7,[3])-(3,[3,2,2])-XxIx>
--
-- >>> foldr del ejemplo1 [5,2,5,3,3]
-- (8,[2])-XxIx>
--
-- >>> del 1 ejemplo2
-- (2,[2,1,3,2])-(4,[4,2])-XxIx>
--
-- >>> del 5 ejemplo2
-- (1,[2,1,1,3,2])-(4,[4,2])-*** Exception: No se ha encontrado ese peso en el tren

-- =====================================================

-- DO NOT MODIFY CODE ABOVE

del :: Int -> Tren -> Tren
del x Maquina = error "No se ha encontrado ese peso en el tren"
del x (Vagon cr ys tren)
  | length newPesos == length ys = Vagon cr ys (del x tren)
  | otherwise = if null newPesos then tren else Vagon (cr+x) newPesos tren
    where newPesos = borraPeso x ys

borraPeso :: Int -> [Int] -> [Int]
borraPeso x [] = []
borraPeso x (y:ys) = if x == y then ys else y: borraPeso x ys 

-- DO NOT MODIFY CODE BELOW

-- =====================================================

instance Show Tren where
    show Maquina         = "XxIx>"
    show (Vagon c xs rt) = concat ["(",show c,",",show xs,")","-",show rt]
