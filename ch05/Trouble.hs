import Data.Char (toUpper)

upcaseFirst (c:cs) = toUpper c --forgot ":cs" here

camelCase :: String -> String
--compiling errors because upcaseFirst isn't typed how we expect it.
camelCase = concatMap upcaseFirst . words
