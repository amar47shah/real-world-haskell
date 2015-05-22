str2action :: String -> IO ()
str2action input = putStrLn $ "Data: " ++ input

list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2actions strings

printItAll :: IO ()
printItAll = runAll actions

-- Take a list of actions and execute each of them in turn.
runAll :: [IO ()] -> IO ()
runAll [] = return ()
runAll (firstElem:remainingElems) =
  do firstElem
     runAll remainingElems

main = do str2action "Start of the program"
          printItAll
          str2action "Done!"
