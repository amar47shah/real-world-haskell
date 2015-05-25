module GlobRegexExercise
  (
    globToRegex
  , matchesGlob
  ) where

import Data.Char(isLower, isUpper, toLower, toUpper)
import Text.Regex.Posix ((=~))

globToRegex :: Bool -> String -> String
globToRegex caseSensitive = bracket ('^', '$') . (case caseSensitive of
                                    True -> globToRegex'
                                    False -> globToInsensitiveRegex')

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob caseSensitive name pat = name =~ globToRegex caseSensitive pat

globToInsensitiveRegex' :: String -> String
globToInsensitiveRegex' ""             = ""
globToInsensitiveRegex' ('*':cs)       = ".*" ++ globToInsensitiveRegex' cs
globToInsensitiveRegex' ('?':cs)       = '.'  :  globToInsensitiveRegex' cs
globToInsensitiveRegex' ('[':'!':c:cs) = "[^" ++ c : caseCounterpart c : insensitiveCharClass cs
globToInsensitiveRegex' ('[':c:cs)     = '['  :  c : caseCounterpart c : insensitiveCharClass cs
globToInsensitiveRegex' ('[':_)        = error "unterminated character class"
globToInsensitiveRegex' (c:cs)         = '[' : escapeInsensitive c ++ "]" ++
                                         globToInsensitiveRegex' cs

globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.'  :  globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]

escapeInsensitive :: Char -> String
escapeInsensitive c | c `elem` regexChars = '\\' : [c]
                    | otherwise           = c : caseCounterpart c : []

regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass ""       = error "unterminated character class"

insensitiveCharClass :: String -> String
insensitiveCharClass (']':cs) = ']' : globToInsensitiveRegex' cs
insensitiveCharClass (c:cs)   = c : caseCounterpart c : insensitiveCharClass cs
insensitiveCharClass ""       = error "unterminated character class"

bracket :: (Char, Char) -> String -> String
bracket (x, y) cs = x : cs ++ y : ""

caseCounterpart :: Char -> Char
caseCounterpart c
  | isLower c = toUpper c
  | isUpper c = toLower c
  | otherwise = c
