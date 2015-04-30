-- Chapter 4, Section 1, Exercise 4
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = transposeTwoLines

transposeTwoLines :: String -> String
transposeTwoLines input = unlines . map (\(x, y) -> [x, y]) . uncurriedZip $ firstTwoLines input

uncurriedZip :: ([a], [b]) -> [(a, b)]
uncurriedZip = uncurry zip

firstTwoLines :: String -> (String, String)
firstTwoLines input =
  let lines = splitLines input
  in (head $ lines, head . tail $ lines)

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'
