import Data.Char (digitToInt)
import Data.List (foldl', isInfixOf)

-- Chapter 4, Section 1, Exercise 1
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit (_:[]) = Just []
safeInit (x:xs) = Just (x : case safeInit xs of
                                 Nothing -> []
                                 Just ys -> ys)

-- Chapter 4, Section 1, Exercise 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = case dropWhile f xs of
                      [] -> []
                      ys -> z : splitWith f zs
                            where (z, zs) = break f ys

-- Chapter 4, Section 2, Exercise 1
asInt_fold :: String -> Int
asInt_fold cs
  | "." `isInfixOf` cs = error "Decimals not allowed"
  |     18 < length cs = error "Number too large"
asInt_fold ""          = error "No digits in string"
asInt_fold ('-':cs)    = -1 * asInt_fold cs
asInt_fold      cs     = fst $ foldr step (0, 1) cs
  where step c (sum, power) = (sum + power * digitToInt c, power * 10)

-- Chapter 4, Section 2, Exercise 2
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either cs
  | "." `isInfixOf` cs  = Left "Decimals not allowed"
  |     18 < length cs  = Left "Number too large"
asInt_either    ""      = Left "No digits in string"
asInt_either   ('-':cs) = case asInt_either cs of
                            Right int -> Right (-1 * int)
                            Left  err -> Left  err
asInt_either        cs  = Right (fst $ foldr step (0, 1) cs)
  where step c (sum, power) = (sum + power * digitToInt c, power * 10)

-- Chapter 4, Section 2, Exercise 3
myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- Chapter 4, Section 2, Exercise 4
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
  | f x              = x : myTakeWhile f xs
  | otherwise        = []
myTakeWhile f _      = []

myTakeWhile_fold :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fold f xs = fst $ foldl' step ([], True) xs
  where step (taken, taking) x
          | taking && f x = (taken ++ [x], True )
          | otherwise     = (taken       , False)

-- Chapter 4, Section 2, Exercise 5
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f xs = fst $ foldl' step ([], xs) xs
  where step (groups, xs) x
          | null run  = (groups         , rest)
          | otherwise = (groups ++ [run], rest)
          where (run, rest) = span (f x) xs

-- Chapter 4, Section 2, Exercise 6
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x b -> f x || b) False

myAny_left :: (a -> Bool) -> [a] -> Bool
myAny_left f = foldl' (\b x -> b || f x) False
-- either is acceptable, since OR is both right- and left-associative

myCycle :: [a] -> [a]
myCycle [] = error "Empty list"
myCycle xs = foldr (\_ l -> xs ++ l) [] [1..]

myWords :: String -> [String]
myWords s = fst $ foldl' step ([], s) s
  where step (words, s) c
          | null word = (words          , trim rest)
          | otherwise = (words ++ [word], trim rest)
          where (word, rest) = break isSpace s
                trim         = dropWhile isSpace
                isSpace      = (== ' ')

myLines :: String -> [String]
myLines s = fst $ foldl' step ([], s) s
  where step (lines, s) c
          | null line && (null rest || head rest /= '\n')
                      = (lines          , trim rest)
          | otherwise = (lines ++ [line], trim rest)
          where (line, rest)   = break isLineBreak s
                isLineBreak    = (== '\n')
                trim ('\n':cs) = cs
                trim       cs  = cs
