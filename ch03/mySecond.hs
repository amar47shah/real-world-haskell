--mySecond :: [a] -> a
--mySecond []     = error "empty list"
--mySecond (_:[]) = error "list too short"
--mySecond (_:xs) = head xs

mySecond :: [a] -> Maybe a
mySecond (_:x:_) = Just x
mySecond _       = Nothing
