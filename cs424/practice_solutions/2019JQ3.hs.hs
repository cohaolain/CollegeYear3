scatterGather :: v -> [Int] -> [v] -> [v]
scatterGather place i v =
    map (\ti ->
        if (length v) > ti
        then (last $ take (ti + 1) v)
        else place)
    i