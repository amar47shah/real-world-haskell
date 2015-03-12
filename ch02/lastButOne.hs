--lastButOne [1, 2, 3] == 2
--lastButOne "abcde" == 'd'
--lastButOne "a" --> Exception: not enough elements
--lastButOne []  --> Exception: not enough elements
lastButOne :: [a] -> a
lastButOne l = if length l > 1
               then (head . tail . reverse) l
               else error "not enough elements"

lastButOne' :: [a] -> a
lastButOne' l = if length l > 1
                then if length l == 2
                     then head l
                     else lastButOne' (tail l)
                else error "not enough elements"

lastButOne'' :: [a] -> a
lastButOne'' l = if length l > 2
                 then lastButOne'' (tail l)
                 else if length l == 2
                      then head l
                      else error "not enough elements"
