transpose :: [[a]] -> [[a]]
transpose l = if length (head l) /= 0 then (map head l):(transpose (map tail l)) else []

main = do
    print $ transpose [[1,2,3], [4,5,6], [7,8,9]]