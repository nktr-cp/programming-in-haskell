euclid :: Int -> Int -> Int
euclid x y | x `mod` y == 0 = y 
euclid x y                  = euclid y (x `mod` y)

{-
textbook
euclid x y | x == y = x
           | x < y  = euclid x (y-x)
           | y < x  = euclid (x-y) x
-}