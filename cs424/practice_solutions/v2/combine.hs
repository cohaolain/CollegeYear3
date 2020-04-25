foo :: [a] -> [a] -> [a]
foo a b = 
    let
        bar a b n =
            if (sum $ map length [a,b]) /= 0
                then
                    (if n `mod` 2 == 1
                                        then (take (min (length a) n) a) ++ bar (drop (min (length a) n) a) b (n + 1)
                                        else (take (min (length b) n) b) ++ bar a (drop (min (length b) n) b) (n + 1))
                else []
    in
        bar a b 1

main = do
    print $ foo [1,2,3,4,5,6,7,8] [11,12,13,14,15,16,17,18]