-- lines beginning with "--" are comments

--original
--main = interact wordCount
--  where wordCount input = show (length (lines input)) ++ "\n" 

--counting words
--main = interact wordCount
--  where wordCount input = show (length (words input)) ++ "\n"

--counting characters
main = interact characterCount
  where characterCount input = show (length input) ++ "\n"
