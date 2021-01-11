module RoseTree where

-- árboles generales en Haskell (rose trees)

data Tree a = Empty | Node a [Tree a] deriving Show

gtree1 :: Tree Int
gtree1 =
  Node 1 [ Node 2 [ Node 4 [ ]
                  , Node 5 [ ]
                  , Node 6 [ ]
                  ]
         , Node 3 [ Node 7 [ ] ]
         ]

sumT :: Num a => Tree a -> a
sumT Empty = 0
sumT ( Node x t ) = x + sum [ sumT s | s <- t]

heightT :: Tree a -> Int
heightT Empty = 0
heightT ( Node x t ) = 1 + maximum [ heightT s | s <- t ] 
