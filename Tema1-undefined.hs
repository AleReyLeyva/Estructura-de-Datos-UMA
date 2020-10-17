-------------------------------------------------------------------
-- Estructuras de Datos
-- Grado en Ingeniería Informática, del Software y de Computadores
-- Tema 1. Introducción a la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema1 where

-- Haskell = Puro + Tipificado estático fuerte + Perezoso

-- Puro: los datos son inmutables (como las String de Java)

-- Tipificado estático fuerte:
--    el tipo se establece en tiempo de compilación y no varía
--    no se pueden mezclar tipos distintos
--    no hay conversión implícita entre tipos (ni siquiera números)

-- Perezoso: sólo se evalúa lo necesario

-- ejemplos de pereza:

-- la función "undefined" provoca un error, pero en los siguientes
-- ejemplos no siempre es necesario evaluar "undefined":

lista = [1,2,3,4]
listaUndef = [1,2,undefined,4] -- error
sumaLista = sum [1,2,3,4]
sumaUndef = sum [1,2,undefined,4] -- error
longitudLista = length [1,2,3,4]
longitudUndef = length [1,2,undefined,4] -- funciona a pesar de undefined

{-

Ejercicios de currificación

    f(x+y)

    f(x+1,y)

    f(2*x,y+1)

    g(f(x,y))

    g(x)+y

    f(x,y) + u*v

    max(max(x,y+1), max(z,-z))
-}

{-

Ejercicios de descurrificación

    f 2 + 3 - g x y + 7

    g (2+x) y * 6

    f g x

    f (g x)

    f f f x

    max (abs (-7)) 6*x

-}

-- Definición de funciones en Haskell

-- |
-- >>> twice 3
-- 6

{-

   int twice(int x) {
      return x + x;
   }

-}

twice = undefined

-- |
-- >>> second 7 3
-- 3

{-

   int second(int x, int y) {
      return y
   }

-}

second = undefined

-- |
-- >>> square 5
-- 25

{-

   int square(int x) {
      return x*x;
   }

-}

square = undefined

-- |
-- >>> pythagoras 2 5
-- 29

{-

   int pythagoras(int x, int y) {
      return square(x) + square(y);
   }

-}

pythagoras = undefined

-- el condicional if then else en Haskell es una expresión, no una sentencia
-- el else es obligatorio
-- los tipos de then y else deben coincidir

-- |
-- >>> máximo 7 3
-- 7
-- >>> máximo 6 2
-- 6

máximo :: Int -> Int -> Int -- predefinida como max
máximo x y = undefined

-- |
-- >>> máximoDeTres 7 9 2
-- 9
-- >>> máximoDeTres 17 6 12
-- 17

máximoDeTres :: Int -> Int -> Int -> Int
máximoDeTres x y z = undefined

-- |
-- >>> signo 6
-- 1
-- >>> signo (-4)
-- -1
-- >>> signo 0
-- 0

{-

   int signo(int x) {
      if (x == 0) return  0;
      if (x <  0) return -1;
      if (x >  0) return  1;
   }

-}

signo :: Integer -> Integer --  predefinida como signum
signo x = undefined

-- Guardas: a partir de 3 o más casos es preferible emplear
-- guardas que anidar if then else

máximoDeTres' :: Int -> Int -> Int -> Int
máximoDeTres' x y z = undefined

signo' :: Integer -> Integer
signo' x = undefined

-- recursión: el factorial

-- |
-- >>> factorial 5
-- 120
-- >>> factorial 0
-- 1

factorial :: Int -> Int
factorial x = if x <= 0 then 1
              else x * factorial (x-1)


-- operadores

-- una función binaria (div, max, ...) se puede usar como operador
--
--   div x y == x `div` y
--   max x y == x `max` y

-- función max usada como operador: `max`
máximoDeTres'' :: Int -> Int -> Int -> Int
máximoDeTres'' x y z = x `max` y `max` z

-- órdenes de reducción

-- tuplas

-- el número de componentes fijo
-- cada componente puede ser de un tipo distinto
-- es un producto cartesiano
-- existe la tupla vacía: ()
-- no existe la tupla unitaria: (x)
-- utilidad de las tuplas: una función puede devolver varios datos

-- |
-- >>> sucPred 5
-- (4,6)

sucPred :: Integer -> (Integer, Integer)
sucPred x = undefined

-- |
-- >>> códigosDe 'a'
-- (65,97)

códigosDe :: Char -> (Int, Int)
códigosDe x = undefined

{-

Ejercicios de tipado de tuplas:

Suponiendo que las expresiones enteras son de tipo Integer y las
expresiones reales son de tipo Double, tipar las siguientes
expresiones:


(2 `div` 3, True, 'z')

( 1 < 2, if 5 < 7 then 'a' else 'b', -1, (7, 'c'))

(8, (), toLower 'A')

(8, ('a'), toLower 'A')

(8, (()), toLower 'A')

( (ord 'a', ord 'z') , (ord 'A', ord 'Z') )

-}

-- versiones monomórficas (tipos concretos)

-- |
-- >>> primeroI (6,8)
-- 6

primeroI :: (Integer, Integer) -> Integer
primeroI (x,y) = undefined

-- |
-- >>> primeroC ('f','p')
-- 'f'

primeroC :: (Char, Char) -> Char
primeroC (x,y) = undefined

-- |
-- >>> primeroB ( 1 < 2, 2 > 3)
-- True

primeroB :: (Bool, Bool) -> Bool
primeroB (x,y) = undefined

-- Versiones polimórficas (variables de tipo)

-- lo que Java llama genericidad en Haskell se llama polimorfismo
-- si un tipo empieza por minúscula es una variable de tipo
-- en Java se escribe <T>, en Haskell se escribe t
-- suelen utilizarse las primeras letras del alfabeto: a,b,c

-- versiones polimórficas de las funciones anteriores
-- el código es el mismo, sólo cambia el tipo

-- |
-- >>> primero (6,8)
-- 6
-- >>> primero ('f','p')
-- 'f'
-- >>> primero ( 1 < 2, 2 > 3)
-- True

-- predefinida como fst
primero (x,y) = undefined

-- |
-- >>> segundo (6,8)
-- 8
--
-- >>> segundo ('f','p')
-- 'p'
--
-- >>> segundo ( 1 < 2, 2 > 3)
-- False

-- predefinida como snd
segundo (x,y) = undefined

-- Polimorfismo restringido: el sistema de clases

-- en Java el genérico <T> puede ser reemplazado por cualquier tipo (clase)
-- en Haskell una variable de tipo a puede ser reemplazada por cualquier tipo
-- en Java puedo restringir un genérico: T extends Comparable<T>
-- en Haskell puedo restringir una variable de tipo: Ord a => a
-- una clase Haskell es semejante (pero no equivalente) a una interfaz Java

-- la clase Eq

-- |
-- >>> sonSimétricos (6,8) (8,6)
-- True
-- >>> sonSimétricos ('a','t') ('s','a')
-- False

-- mismo tipo
sonSimétricos :: (a,a) -> (a,a) -> Bool
sonSimétricos (x,y) (u,v) = undefined

-- |
-- >>> sonSimétricos' (6,'a') ('a',6)
-- True
-- >>> sonSimétricos ('a',True) (False,'a')
-- False

-- tipos distintos
sonSimétricos' :: (a,b) -> (b,a) -> Bool
sonSimétricos' (x,y) (u,v) = undefined

-- |
-- >>> estáOrdenada (6,8)
-- True
-- >>> estáOrdenada ('c','a')
-- False

-- la clase Ord
estáOrdenada :: (a,a) -> Bool
estáOrdenada (x,y) = undefined

-- la clase Num

-- |
-- >>> cubo 3
-- 27

cubo :: Num a => a -> a
cubo x = undefined

-- la clase Integral: divisón entera

-- |
-- >>> esMútiplo 12 3
-- True
-- >>> esMútiplo 7 3
-- False

esMúltiplo :: a -> a -> Bool
esMúltiplo nx x = undefined

-- la clase Fractional: división con decimales

-- |
-- >>> inversoNoNulo 2
-- 0.5

inversoNoNulo :: a -> a
inversoNoNulo x = undefined

-- la función error es como un throw de Java simplificado

-- |
-- >>> inverso 2
-- 0.5

inverso :: Double -> Double
inverso x = undefined

-- Definiciones locales con where

raíces :: Double -> Double -> Double -> (Double, Double)
raíces a b c = ((-b + sqrt (b*b-4*a*c)) / (2*a),
                (-b - sqrt (b*b-4*a*c)) / (2*a))

-- mejora la modularidad, la legibilidad y la eficiencia

-- raíces 2 7 1 - dos reales
-- raíces 2 4 2 - una real
-- raíces 2 0 1 - dos complejas
-- raíces 0 1 1 - no es de segundo grado

raíces' :: Double -> Double -> Double -> (Double, Double)
raíces' a b c = undefined

-- regla de sangrado

circArea :: Double -> Double
circArea r = pi*r^2

rectArea :: Double -> Double -> Double
rectArea b h = b*h

circLength :: Double -> Double
circLength r = 2*pi*r

cylinderArea :: Double -> Double -> Double
cylinderArea r h = 2*circ + rect
   where
        circ = circArea r
        l = circLength r
        rect = rectArea l h

-- Definición de operadores

-- |
-- >>> 3.0000001 ~= 3.0
-- True
-- >>> 3.000001 ~= 3.0
-- False

infix ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs(x-y) < 1e-6
