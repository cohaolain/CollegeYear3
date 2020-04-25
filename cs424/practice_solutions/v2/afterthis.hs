{-Q3:

Define a Haskell procedure afterThis which takes a list xs and a second list ys and returns the Just the first element of ys following a stretch of elements equal to xs, or Nothing if that does not exist.  Be sure to give a type signature.

Example

afterThis "oo" "foobar"
 => Just 'b'

afterThis "oo" "fubar"
 => Nothing

afterThis [0,0] [1,2,3,0,0,4,0,0,5]
 => Just 4

afterThis [4,4] [1,2,3,0,0,4,4]
 => Nothing-}

afterThis :: (Eq a) => [a] -> [a] -> Maybe a
afterThis s v
    | (s == (take (length s) v)) && (length v > length s)
    = Just $ v !! length s
afterThis xs (y:ys) = afterThis xs ys
afterThis _ [] = Nothing

main = do
    print $ afterThis [0,0] [1,2,3,0,0,4,0,0,5]