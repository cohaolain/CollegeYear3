mapskip :: (a -> a) -> [a] -> [a]
mapskip f l
    | lp == 2 = [f (head pair), last pair] ++ mapskip f rest
    | lp == 1 = [f (head pair)]
    | otherwise = []
    where
        (pair, rest) = splitAt 2 l 
        lp = (length pair)

main = do
    print $ mapskip (\x -> x + 2) [1,3,5,7,7]