myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

--test
--and (map (\l -> length l == myLength l) [[], "a", "ab", "abc", "abcd", "abcde"])
--and (map (\l -> length l == myLength l) [[], [1], [1,2], [1..3], [1..4], [1..5]])
--checks equivalence of length and myLength for all inputs in list of lists.
--result should be true

myLength' :: [a] -> Int
myLength' xs = foldl (+) 0 (map (\_ -> 1) xs)
