import Data.Char
-- 7.1
add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2
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

-- 7.3
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

-- 演算子には関数も指定できる
length' :: [a] -> Int
length' = foldr (\_ n -> 1+n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

-- 7.4
sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

-- 第一引数が蓄積変数
length'' :: [a] -> Int
length'' = foldl (\n _ -> n+1) 0

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- 7.5
(.#) :: (b -> c) -> (a -> b) -> (a -> c)
f .# g = \x -> f (g x)

odd' :: Int -> Bool
odd' = not . even

twice' :: (a -> a) -> a -> a
twice' f = f . f

sumsqreven' :: [Int] -> Int
sumsqreven' = sum . map (^2) . filter' even

id' :: a -> a
id' = \x -> x

-- 関数のリストをとり、それらを合成したものを返す
compose :: [a -> a] -> (a -> a)
compose = foldr' (.) id'

-- 7.6
type Bit = Int

{-
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weight bits]
               where weight = iterate (*2) 1
-}

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + y*2) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7.7
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)