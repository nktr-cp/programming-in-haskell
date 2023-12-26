add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
-}

-- recursion
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = [f x] ++ map' f xs
-- map' f (x:xs) = f x : map f xs

{-
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]
-}

-- recursion
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []                 = []
fliter' p (x:xs) | p x       = x : filter p xs
                 | otherwise = filter' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- foldr
{-
sum' :: Num a => [a] -> a
sum' = foldr (+) 0
{- this is equivalent to:
sum' []     = 1
sum' (x:xs) = x * product xs
-}

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- λ式を使うと拡張性が上がる
length' :: [a] -> Int
length' = foldr (\_ n -> 1+n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []
-}

-- foldl
sum' :: Num a => [a] -> a
sum' = foldl (+) 0

-- ここまだよく理解してない
length' :: [a] -> Int
length' = foldl (\n _ -> n+1) 0