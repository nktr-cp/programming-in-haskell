fac :: Int -> Int
-- 基底部
fac 0 = 1
-- 再帰部
fac n = if n > 0 then n * fac(n-1) else -1
-- textbook
-- execptionを出したいならこっち
-- fac n | n > 0 = n * fac(n-1)