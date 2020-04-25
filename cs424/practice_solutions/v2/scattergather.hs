scattergather :: a -> [Int] -> [a] -> [a]
scattergather p i v =
    map (\ti ->
            if ti < length v
                then (last $ take (ti + 1) v)
                else p)
        i
 