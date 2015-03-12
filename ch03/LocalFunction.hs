pluralize :: String -> [Int] -> [String]
pluralize word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "1 " ++ word
          plural n = show n ++ " " ++ word ++ "s"
