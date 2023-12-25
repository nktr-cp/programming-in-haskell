grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]