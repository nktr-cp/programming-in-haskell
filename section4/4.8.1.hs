halve :: [a] -> ([a],[a])
halve a = (take n a, drop n a)
  where n = length a `div` 2