myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl step acc (x:xs) = myFoldl step (step acc x) xs
myFoldl _    acc _      = acc

foldlSum :: [Double] -> Double
foldlSum = foldl step 0
  where step acc x = acc + x

niceSum :: [Double] -> Double
niceSum = foldl (+) 0

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr step acc (x:xs) = step x (myFoldr step acc xs)
myFoldr _    acc _      = acc

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr step []
  where step x ys | p x       = x : ys
                  | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr step []
  where step x ys = f x : ys

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f z xs = foldr step id xs z
  where step x g a = g (f a x)

identity :: [a] -> [a]
identity = foldr (:) []

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

foldl' _    zero []     = zero
foldl' step zero (x:xs) = let new = step zero x
                          in  new `seq` foldl' step new xs
--   0) foldl' (+) 1 (2:[])
--   1) let new = 1 + 2 in new `seq` foldl' (+) new []
--   2) foldl' (+) 3 []
--   3) 3

-- Refer to Chapter 4's final section, "Learning to use seq"
-- for more info
