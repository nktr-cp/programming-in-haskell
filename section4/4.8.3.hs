safetail :: [a] -> [a]
-- a
-- safetail xs = if null xs then [] else tail xs
-- b
-- safetail xs | null xs = []
--             | otherwise = tail xs
-- c
safetail [] = []
safetail xs = tail xs