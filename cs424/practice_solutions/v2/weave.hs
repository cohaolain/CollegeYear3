weaveHunks :: Int -> [a] -> [a] -> [a]
weaveHunks n a b = 
    (take n a) ++ (take n b) ++ (if sum (map length [a,b]) /= 0 then (weaveHunks n (drop n a) (drop n b)) else [])

main = do
    print $ weaveHunks 3 [1..10] [11..20]