quux a = let a = "foo"
         in a ++ "eek!"

lend2 amount balance =
    case balance of
         Nothing    -> Nothing
         Just value -> if newBalance >= reserve
                       then Just newBalance
                       else Nothing
           where reserve    = 100
                 newBalance = value - amount
