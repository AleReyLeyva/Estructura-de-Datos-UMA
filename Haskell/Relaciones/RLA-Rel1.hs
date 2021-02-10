-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Doble Grado de Matemáticas e Ingeniería Informática
-- Alumno: Rey Leyva, Alejandro
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: 1-17
--
-------------------------------------------------------------------------------
import Test.QuickCheck

-- 1.a
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = if (x ^ 2 + y ^ 2 == z ^ 2) then True else False

-- 1.b
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y
  | x > y = (x ^ 2 - y ^ 2, 2 * x * y, x ^ 2 + y ^ 2)
  | otherwise = error "ERROR. x debe ser mayor que y"

-- 1.c Leido

-- 1.d QuickCheck
p_ternas x y = x > 0 && y > 0 && x > y ==> esTerna l1 l2 h
  where
    (l1, l2, h) = terna x y

-- 2
intercambia :: (a, b) -> (b, a)
intercambia (a, b) = (b, a)

-- 3.a
ordena2 :: Ord a => (a, a) -> (a, a)
ordena2 (a, b)
  | a > b = (b, a)
  | otherwise = (a, b)

p1_ordena2 x y = enOrden (ordena2 (x, y))
  where
    enOrden (x, y) = x <= y

p2_ordena2 x y = mismosElementos (x, y) (ordena2 (x, y))
  where
    mismosElementos (x, y) (z, v) = (x == z && y == v) || (x == v && y == z)

-- 3.b
ordena3 :: Ord a => (a, a, a) -> (a, a, a)
ordena3 (x, y, z)
  | x > y = ordena3 (y, x, z)
  | y > z = ordena3 (x, z, y)
  | otherwise = (x, y, z)

p1_ordena3 x y z = enOrden (ordena3 (x, y, z))
  where
    enOrden (x, y, z) = x <= y && y <= z

p2_ordena3 x y z = mismosElementos (x, y, z) (ordena3 (x, y, z))
  where
    mismosElementos (x, y, z) (j, k, l) = (x == j && y == k && z == l) || (x == k && y == l && z == j) || (x == l && y == j && z == k) || (x == j && y == l && z == k) || (x == k && y == j && z == l) || (x == l && y == k && z == j)

-- 4.a
max2 :: Ord a => a -> a -> a
max2 x y
  | x > y = x
  | otherwise = y

-- 4.b
p1_max2 x y = biendef (max2 x y)
  where
    biendef a = a == x || a == y

p2_max2 x y = mayorigual (max2 x y)
  where
    mayorigual a = a >= x && a >= y

p3_max3 x y = if (x >= y) then xmiy (max2 x y) else True
  where
    xmiy a = a == x

p4_max3 x y = if (x <= y) then ymix (max2 x y) else True
  where
    ymix a = a == y

-- 5
entre :: Ord a => a -> (a, a) -> Bool
entre x (y, z) = x >= y && x <= z

-- 6
iguales3 :: Eq a => (a, a, a) -> Bool
iguales3 (x, y, z) = x == y && y == z

-- 7.a
type TotalSegundos = Integer

type Horas = Integer

type Minutos = Integer

type Segundos = Integer

descomponer :: TotalSegundos -> (Horas, Minutos, Segundos)
descomponer x = (horas, minutos, segundos)
  where
    horas = (div x 3600)
    minutos = (div (mod x 3600) 60)
    segundos = (mod (mod x 3600) 60)

-- 7.b
p_descomponer x =
  x >= 0 ==> h * 3600 + m * 60 + s == x
    && entre m (0, 59)
    && entre s (0, 59)
  where
    (h, m, s) = descomponer x

-- 8
unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x = x / unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x * unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) == x

-- 9
infix 4 ~=

(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) < epsilon
  where
    epsilon = 1 / 1000

-- 10
raíces :: Double -> Double -> Double -> (Double, Double)
raíces a b c
  | ((b ^ 2 - (4 * a * c)) < 0) = error "Polinomio sin raíces reales"
  | otherwise = ((((- b) + sqrt (b ^ 2 - (4 * a * c))) / (2 * a)), (((- b) - sqrt (b ^ 2 - (4 * a * c))) / (2 * a)))

p1_raíces a b c = esRaíz r1 && esRaíz r2
  where
    (r1, r2) = raíces a b c
    esRaíz r = a * r ^ 2 + b * r + c ~= 0

p2_raíces a b c = (a /= 0) && ((b ^ 2 - (4 * a * c)) >= 0) ==> esRaíz r1 && esRaíz r2
  where
    (r1, r2) = raíces a b c
    esRaíz r = a * r ^ 2 + b * r + c ~= 0

-- 11
esMúltiplo :: (Integral a) => a -> a -> Bool
esMúltiplo x y
  | (mod x y == 0) = True
  | otherwise = False

-- 12
infixl 1 ==>>

(==>>) :: Bool -> Bool -> Bool
False ==>> y = True
True ==>> False = False
True ==>> True = True

-- 13
esBisiesto :: Integer -> Bool
esBisiesto x = (mod x 4 == 0) && ((mod x 100 == 0) ==>> (mod x 400 == 0))

-- 14.a
potencia :: Integer -> Integer -> Integer
potencia x y
  | y < 0 = error "El exponente debe ser positivo"
  | y == 0 = 1
  | otherwise = x * (potencia x (y -1))

-- 14.b
potencia' :: Integer -> Integer -> Integer
potencia' x y
  | y < 0 = error "El exponente debe ser positivo"
  | y == 0 = 1
  | (mod y 2 == 0) = ((potencia' x (div y 2)) ^ 2)
  | (mod y 2 /= 0) = (x * ((potencia' x (div (y -1) 2)) ^ 2))

-- 14.c
p_pot b n =
  n >= 0 ==> potencia b n == sol
    && potencia' b n == sol
  where
    sol = b ^ n

-- 14.d
-- potencia -> n-1 productos
-- potencia' -> n/2 productos

-- 15
factorial :: Integer -> Integer
factorial x
  | x < 0 = error "El número debe ser mayor o igual q cero"
  | x == 0 = 1
  | otherwise = x * factorial (x -1)

-- 16.a
divideA :: Integer -> Integer -> Bool
divideA x y
  | x /= 0 = mod y x == 0
  | otherwise = error "El denominador debe ser distinto 0"

--16.b QuickCheck Comprobado
p1_divideA x y = y /= 0 && y `divideA` x ==> div x y * y == x

--16.c
p2_divideA x y z = x /= 0 && (x `divideA` y) && (x `divideA` z) ==> x `divideA` (y + z)

--17
mediana :: (Ord a) => (a, a, a, a, a) -> a
mediana (x, y, z, t, u)
  | x > z = mediana (z, y, x, t, u)
  | y > z = mediana (x, z, y, t, u)
  | u < z = mediana (x, y, u, t, z)
  | t < z = mediana (x, y, t, z, u)
  | otherwise = z
