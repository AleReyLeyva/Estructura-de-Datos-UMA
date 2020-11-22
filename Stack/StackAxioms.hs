------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo de la especificación del TAD Stack
------------------------------------------------------------

module StackAxioms where

import           Stack
-- import           DataStructures.Stack.LinearStack
-- import           DataStructures.Stack.StackOnList

import           Test.QuickCheck

------------------------------------------------------------
-- Especificación del TAD Stack
------------------------------------------------------------

-- Las operaciones del TAD se clasifican en constructores,
-- selectores y transformadores.

-- Para especificar un TAD hay que especificar qué devuelve
-- cada selector y transformador cuando se aplica a un
-- constructor.

-- Para el TAD Pila tenemos 2 constructores: push y empty.
-- El resto de operaciones son selectores (isEmpty, top)
-- o transformadores (pop).

-- Definimos una propiedad QuickCheck para comprobar cada
-- operación sobre el TAD. A estas propiedades las llamamos
-- axiomas de la pila. Cualquier implementación de la pila
-- debe satisfacer los axiomas.

-- ¿Qué devuelve isEmpty cuando se aplica a un constructor?

-- isEmpty :: Stack a -> Bool
ax_isEmpty_empty = isEmpty empty == True
ax_isEmpty_push x s = isEmpty (push x s) == False

-- ¿Qué devuelve top cuando se aplica a un constructor?

-- top :: Stack a -> a
ax_top_push x s = top (push x s) == x
-- top sobre empty no está definido: depende de la implementación

-- ¿Qué devuelve pop cuando se aplica a un constructor?

-- pop :: Stack a -> Stack a
ax_pop_push x s = pop (push x s) == s
-- pop sobre empty no está definido: depende de la implementación

type T = Integer -- o Char, etc.

check_isEmpty_empty = quickCheck (ax_isEmpty_empty :: Bool)
check_isEmpty_push  = quickCheck (ax_isEmpty_push :: T -> Stack T -> Bool)
check_pop_push      = quickCheck (ax_pop_push :: T -> Stack T -> Bool)
check_top_push      = quickCheck (ax_top_push :: T -> Stack T -> Bool)

-- Para comprobar todas las propiedades QuickCheck.

check_Stack = do
                 check_isEmpty_empty
                 check_isEmpty_push
                 check_pop_push
                 check_top_push
