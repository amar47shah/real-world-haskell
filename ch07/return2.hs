import Data.Char(toUpper)

isYes :: String -> Bool
isYes s = (toUpper . head $ s) == 'Y'

isGreen :: IO Bool
isGreen =
  do putStrLn "Is green your favorite color?"
     inpStr <- getLine
     return $ isYes inpStr
