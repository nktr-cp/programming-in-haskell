(^#) :: Int -> Int -> Int
_ ^# 0 = 1
n ^# m = n * (n ^# (m-1))

{-
2 ^ 3
= 2 * (2^(3-1))
= 2 * (2 * 2^(2-1))
= 2 * (2 * 2 * 2^(1-1))
= 2 * (2 * 2 * 1)
= 8
-}