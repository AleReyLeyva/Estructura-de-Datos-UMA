module DictDemos where

import           DataStructures.Dictionary.AVLDictionary

-- En la implementación del dccionario usamos:
--
--   1) constructores de datos infijos
--   2) expresiones case

-- constructor de datos prefijo Par
data Par a b = Par a b

p :: Par Int String
p = Par 3 "Java"

-- constructor de datos infijo :->
data Rel a b = a :-> b

r :: Rel Int String
r =  5 :-> "Haskell"

-- expresiones case (similar a switch)

-- hasta ahora la correspondencia de patrones está limitada a definiones de función

ejemplo :: [a] -> String
ejemplo []      = "vacia"
ejemplo [_]     = "unitaria"
ejemplo (_:_:_) = "más de uno"

-- case permite que la correspondencia de patrones aparezca en cualquier expresión

ejemploCase :: [a] -> String
ejemploCase xs =
    case xs of
      []      -> "vacia"
      [_]     -> "unitaria"
      (_:_:_) -> "más de uno"

-- case es una expresión y puede aparecer en cualquier sitio donde pueda aparecer una expresión
lista :: Integer -> [Integer]
lista x = [1, 2, case x of
                      0 -> 0
                      y -> y + 1,
            5, 6]

caseRel :: Rel Int b -> String
caseRel (x :-> _) =
  case x `mod` 3 of
    0 -> "múltiplo de 3"
    _ -> "otro caso"


mkDict :: Ord a => [(a,b)] -> Dictionary a b
mkDict xs = foldr (\ (x,y) d -> insert x y d) empty xs

d :: Dictionary Integer String
d = mkDict [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
