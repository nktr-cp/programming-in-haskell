(||) :: Bool -> Bool -> Bool
-- 1
-- False || False = False
-- True || False = True
-- False || True = True
-- True || True = True
-- 2
-- False || b = b
-- True || _ = True
-- 3
-- False || False = False
-- _ || __ = True
-- 4
b || c | b == c = b
       | otherwise = True