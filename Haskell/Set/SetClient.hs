module SetClient where

import           Set

mkSet :: Ord a => [a] -> Set a
mkSet xs = foldr insert empty xs

primos :: Set Integer
primos = mkSet [13, 7, 3, 2, 5, 11, 19]

letras :: Set Char
letras = mkSet "abracadabra"

-- |
-- >>> cardinal primos
-- 7
--
-- >>> cardinal letras
-- 5
cardinal :: Set a -> Integer
cardinal s = fold (\ _ resto -> 1 + resto) 0 s

-- |
-- >>> suma primos
-- 60
suma :: Num a => Set a -> a
suma s = fold (+) 0 s

-- |
-- >>> setToList primos
-- [2,3,5,7,11,13,19]
--
-- >>> setToList letras
-- "abcdr"
setToList :: Set a -> [a]
setToList s = fold (:) [] s

-- |
-- >>> filtra even primos
-- Node 2 Empty
--
-- >>> filtra (>10) primos
-- Node 11 (Node 13 (Node 19 Empty))
--
-- >>> filtra (`elem` "aeiou") letras
-- Node 'a' Empty
filtra :: Ord a => (a -> Bool) -> Set a -> Set a
filtra p s
  | isEmpty s = empty
  | otherwise = fold f empty s
      where f x resto = if (p x) then insert x resto else resto

-- |
-- >>> union letras (insert 'A' (insert 'a' (insert 'z' empty)))
-- Node 'A' (Node 'a' (Node 'b' (Node 'c' (Node 'd' (Node 'r' (Node 'z' Empty))))))
union' :: Ord a => Set a -> Set a -> Set a
union' s1 s2
  | isEmpty s1 = s2
  | isEmpty s2 = s1
  | otherwise = fold f s2 s1
      where f x resto = if (isElem x s2) then resto else insert x resto

-- |
-- >>> aplica (*2) primos
-- Node 4 (Node 6 (Node 10 (Node 14 (Node 22 (Node 26 (Node 38 Empty))))))
aplica :: (Ord a, Ord b) => (a->b) -> Set a -> Set b
aplica f s = fold (\ x resto -> insert (f x) resto) empty s

-- |
-- >>> maximo primos
-- Just 19
--
-- >>> maximo letras
-- Just 'r'
maximo :: Ord a => Set a -> Maybe a
maximo s
  | isEmpty s = Nothing
  | otherwise = Just (head (reverse (setToList s)))
