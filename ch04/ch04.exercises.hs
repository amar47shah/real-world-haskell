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

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = case dropWhile f xs of
                      [] -> []
                      ys -> z : splitWith f zs
                            where (z, zs) = break f ys
