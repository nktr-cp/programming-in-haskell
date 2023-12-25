replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

{-
replicate' n x | n == 0 = []
              | otherwise = [x] ++ replicate' (n-1) x
-}