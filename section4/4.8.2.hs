third :: [a] -> a
-- a
-- p.18参照
third x = head(tail(tail x))
-- b
-- third x = x !! 2
-- c
-- third (_:_:x:_) = x