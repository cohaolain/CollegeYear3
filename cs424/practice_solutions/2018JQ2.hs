tear :: (a -> Bool) -> [a] -> [[a]]
tear p a = map (\p' -> filter p' a) [p, not . p]