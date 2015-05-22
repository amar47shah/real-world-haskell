import Data.Char(toUpper)

isGreen :: IO Bool
isGreen =
  do putStrLn "Is green your favorite color?"
     inputStr <- getLine
     return ((toUpper . head $ inputStr) == 'Y')
