-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Doble Grado de Matemáticas e Ingeniería Informática 
-- Alumno: Rey Leyva, Alejandro 
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: 1-... 
--
-------------------------------------------------------------------------------
import Test.QuickCheck

-- 1.a
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = if (x^2 + y^2 == z^2) then True else False

-- 1.b
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y | x > y = (x^2-y^2, 2*x*y, x^2+y^2)
					| otherwise = error "ERROR. x debe ser mayor que y"

-- 1.c Leido

-- 1.d QuickCheck
p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
	 where
		 (l1,l2,h) = terna x y

-- 2
intercambia :: (a,b) -> (b,a)
intercambia (a,b) = (b,a)

-- 3.a
ordena2 :: Ord a => (a, a) -> (a, a)
ordena2 (a, b) | a > b = (b, a)
							 | otherwise = (a, b)

p1_ordena2 x y = enOrden (ordena2 (x,y))
	where enOrden (x,y) = x<=y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
	where
	 mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)

-- 3.b
ordena3 :: Ord a => (a, a, a) -> (a, a, a)
ordena3 (x, y, z)  	| x > y = ordena3 (y, x, z)
										| y > z = ordena3 (x, z, y)	
					 					| otherwise = (x, y, z) 

p1_ordena3 x y z = enOrden (ordena3 (x, y, z))
	where enOrden (x, y, z) = x<=y && y<=z

p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x, y, z))
	where mismosElementos (x, y, z) (a, b, c) =  
