-- Set Three, Exercise 1
module GlobRegexEither
  (
    globToRegex
  , matchesGlob
  ) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> Either GlobError String
globToRegex cs = handleGlobError (globToRegex' cs) (\r -> '^' : r ++ "$")

matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` pat = handleGlobError (globToRegex pat) (\r -> name =~ r)

type GlobError = String

globToRegex' :: String -> Either GlobError String
globToRegex' ""             = Right ""
globToRegex' ('*':cs)       = handleGlobError (globToRegex' cs) (\r -> ".*" ++ r)
globToRegex' ('?':cs)       = handleGlobError (globToRegex' cs) (\r -> '.' : r)
globToRegex' ('[':'!':c:cs) = handleGlobError (charClass cs) (\r -> "[^" ++ c : r)
globToRegex' ('[':c:cs)     = handleGlobError (charClass cs) (\r -> '[' : c : r)
globToRegex' ('[':_)        = Left "unterminated character class"
globToRegex' (c:cs)         = handleGlobError (globToRegex' cs) (\r -> escape c ++ r)

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = handleGlobError (globToRegex' cs) (\r -> ']' : r)
charClass (c:cs)   = handleGlobError (charClass cs) (\r -> c : r)
charClass ""       = Left "unterminated character class"

handleGlobError :: Either GlobError a -> (a -> b) -> Either GlobError b
handleGlobError either f = case either of
                             Left  e -> Left  e
                             Right v -> Right $ f v
