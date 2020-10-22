-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Doble Grado de Matemáticas e Ingeniería Informática
-- Alumno: Rey Leyva, Alejandro
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos:
--
-------------------------------------------------------------------------------
import Test.QuickCheck

-- 1.a
data Direction = North | South | East | West
  deriving (Eq, Enum, Show)

(<<) :: Direction -> Direction -> Bool
x << y = (fromEnum x) < (fromEnum y)

p_menor x y = (x < y) == (x << y)

instance Arbitrary Direction where
  arbitrary = do
    n <- choose (0, 3)
    return $ toEnum n

-- 1.b
instance Ord Direction where
  x <= y = (x << y) || (x == y)

-- 2.a
máximo :: Ord a => [a] -> a
máximo (x : xs)
  | length (x : xs) == 0 = error "La lista debe ser no vacía"
  | length (x : xs) == 1 = x
  | otherwise = if (x > head xs) then máximo (x : (tail xs)) else máximo (xs)

máximoYresto :: Ord a => [a] -> (a, [a])
máximoYresto (x : xs)
  | length (x : xs) == 0 = error "La lista debe ser no vacía"
  | otherwise = (máximo (x : xs), filter (< (máximo (x : xs))) (x : xs))

-- 3

filterIndexParidad :: [a] -> Bool -> [a]
filterIndexParidad (x : xs) par
  | length (x : xs) == 0 = error "La lista debe ser no vacía"
  | length (x : xs) <= 2 = if (par) then (xs) else (x : [])
  | otherwise = if (par) then ((head xs) : (filterIndexParidad (tail xs) par)) else (x : (filterIndexParidad (tail xs) par))

reparte :: [a] -> ([a], [a])
reparte (x : xs)
  | length (x : xs) == 0 = error "La lista debe ser no vacía"
  | otherwise = ((filterIndexParidad (x : xs) False), (filterIndexParidad (x : xs) True))

-- 4
distintos :: Eq a => [a] -> Bool
distintos (x : xs)
  | length (x : xs) == 1 = True
  | otherwise = ((length (filter (/= x) xs)) == (length xs)) && (distintos xs)

-- 5.a
replicate' :: Int -> a -> [a]
replicate' n x = aux [x] n
  where
    aux (x : xs) 0 = []
    aux (x : xs) 1 = (x : xs)
    aux (x : xs) n | n > 0 = aux (x : (x : xs)) (n -1)

-- 5.b
p_replicate' n x =
  n >= 0
    && n <= 1000
      ==> length (filter (== x) xs) == n
    && length (filter (/= x) xs) == 0
  where
    xs = replicate' n x

-- 6
divideA :: Integral a => a -> a -> Bool
divideA x y
  | x == 0 = error "No se puede dividir por 0"
  | otherwise = mod y x == 0

{-
divisores :: Integral a => a -> [a]
divisores a = aux [a] a (a -1)
  where
    aux (x : xs) a 1 = (1 : (x : xs))
    aux (x : xs) a n = if (n `divideA` a) then (aux (n : (x : xs)) a (n -1)) else (aux (x : xs) a (n -1))

divisores' :: Integral a => a -> [a]
divisores' a = aux [a] a (a -1)
  where
    aux (x : xs) a 0 = aux (x : xs) a (-1)
    aux (x : xs) a n | n == (- a) = ((- a) : (x : xs))
    aux (x : xs) a n = if (n `divideA` a) then (aux (n : (x : xs)) a (n -1)) else (aux (x : xs) a (n -1))

-}
divisores :: Integral a => a -> [a]
divisores a = [x | x <- [1 .. a], x `divideA` a]

divisores' :: Integral a => a -> [a]
divisores' a = [x | x <- [- a .. a], (x /= 0) && (x `divideA` a)]

-- 7.a
contains :: Integral a => [a] -> a -> Bool
contains (x : xs) a
  | length (x : xs) == 0 = error "La lista debe ser vacía"
  | length (x : xs) == 1 = (x == a)
  | length (x : xs) > 1 = (x == a) || (contains xs a)

div_común :: Integral a => a -> a -> [a]
div_común x y = [z | z <- (divisores x), (divisores y) `contains` z]

mcd :: Integral a => a -> a -> a
mcd x y
  | (x == 0) && (y == 0) = error "MCD no existente"
  | otherwise = maximum (div_común (abs x) (abs y))

-- 7.b
p_mcd x y z =
  (x /= 0) && (y /= 0) && (z /= 0)
    ==> (mcd (z * x) (z * y)) == (abs z) * (mcd x y)

-- 7.c
mcm :: Integral a => a -> a -> a
mcm x y = div (x * y) (mcd x y)

-- 8.a
esPrimo :: Integral a => a -> Bool
esPrimo x
  | x <= 0 = error "El número debe ser natural"
  | otherwise = (length (divisores x) == 2)

-- 8.b
primosHasta :: Integral a => a -> [a]
primosHasta x = [y | y <- [1 .. x], esPrimo y]

-- 8.c
primosHasta' :: Integral a => a -> [a]
primosHasta' x = filter (esPrimo) ([1 .. x])

-- 8.d
p1_primos x = primosHasta x == primosHasta' x

-- 9.a
pares :: Integral a => a -> [(a, a)]
pares a = [(x, y) | x <- [1 .. (a `div` 2)], y <- [(a `div` 2) .. a], (esPrimo x) && (esPrimo y) && ((x + y) == a)]

-- 9.b
goldbach :: Integral a => a -> Bool
goldbach x = (x > 2) && (not (null (pares x)))

-- 9.c
goldbachHasta :: Integral a => a -> Bool
goldbachHasta x = (length ([y | y <- [3 .. x], (even y) && (goldbach y)])) == (length ([y | y <- [3 .. x], even y]))

-- 9.d
goldbachDébil :: Integral a => a -> Bool
goldbachDébil x = (not (even x)) && (x > 5) && (not (null (pares (x -3))))

goldbachDébilHasta :: Integral a => a -> Bool
goldbachDébilHasta x = (length ([y | y <- [6 .. x], (not (even y)) && (goldbachDébil y)])) == (length ([y | y <- [6 .. x], (not (even y))]))

-- 10.a
esPerfecto :: Integral a => a -> Bool
esPerfecto x = (foldl (+) 0 (filter (/= x) (divisores x))) == x

-- 10.b
perfectosMenoresQue :: Integral a => a -> [a]
perfectosMenoresQue x = [y | y <- [1 .. x], esPerfecto y]

-- 11.a
take' :: Integral a => a -> [a] -> [a]
take' n xs = [x | (p, x) <- zip [0 .. (n -1)] xs]

-- 11.b
drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p, x) <- (zip [1 .. (length xs)] xs), p > (length xs - n)]

-- 11.c
p_take_drop n xs = n >= 0 ==> ((take' n xs) ++ (drop' n xs)) == xs

--12.a
concat' :: [[a]] -> [a]
concat' (x : xs) = foldr (++) [] (x : xs)

--12.b
{-
concat'' :: [[a]] -> [a]
concat'' [[a]] = [y |
-}
