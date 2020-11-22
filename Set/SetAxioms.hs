module SetAxioms where

-- import           DataStructures.Set.SortedLinearSet
import           Set
import           Test.QuickCheck

-- constructores: empty, insert
-- selectores: isEmpty, isElem
-- transformadores: delete

-- ¿Qué devuelve isEmpty cuando se aplica a un constructor?

-- isEmpty :: Set a -> Bool
ax_isEmpty_empty = isEmpty empty == True
ax_isEmpty_insert x s = isEmpty (insert x s) == False

-- ¿Qué devuelve isElem cuando se aplica a un constructor?

-- isElem :: Eq a => a -> Set a -> Bool
ax_isElem_empty x = isElem x empty == False
ax_isElem_insert x y s = isElem x (insert y s) == (x == y || isElem x s)

-- ¿Qué devuelve delete cuando se aplica a un constructor?

-- delete :: Eq a => a -> Set a -> Set a
ax_delete_empty x = delete x empty == empty
ax_delete_insert_1 x y s = x == y ==> delete x (insert y s) == delete x s
ax_delete_insert_2 x y s = x /= y ==> delete x (insert y s) == insert y (delete x s)

-- La instancia de Arbitrary es para enseñar a QuickCheck a
-- generar Set aleatorios: no hay que saber cómo hacerlo;
-- siempre se facilita

-- Comprobaciones QuickCheck de cada axioma con Set T, donde
-- T es un tipo concreto.

type T = Integer -- o Char, etc.

check_isEmpty_empty  = quickCheck (ax_isEmpty_empty :: Bool)
check_isEmpty_insert = quickCheck (ax_isEmpty_insert :: T -> Set T -> Bool)
check_isElem_empty   = quickCheck (ax_isElem_empty :: T -> Bool)
check_isElem_insert  = quickCheck (ax_isElem_insert :: T -> T -> Set T -> Bool)
check_delete_empty   = quickCheck (ax_delete_empty :: T -> Bool)
check_delete_insert_1  = quickCheck (ax_delete_insert_1 :: T -> T -> Set T -> Property)
check_delete_insert_2  = quickCheck (ax_delete_insert_2 :: T -> T -> Set T -> Property)

-- Para comprobar todas las propiedades QuickCheck.

check_Set = do
                 check_isEmpty_empty
                 check_isEmpty_insert
                 check_isElem_empty
                 check_isElem_insert
                 check_delete_empty
                 check_delete_insert_1
                 check_delete_insert_2
