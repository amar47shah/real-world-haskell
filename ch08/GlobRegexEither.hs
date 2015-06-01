module GlobRegexEither
  (
    globToRegex
  , matchesGlob
  ) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> Either GlobError String
globToRegex cs = case globToRegex' cs of
                   Left  e -> Left  e
                   Right r -> Right $ '^' : r ++ "$"

matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` pat = case globToRegex pat of
                           Left  e -> Left  e
                           Right r -> Right $ name =~ r

type GlobError = String

globToRegex' :: String -> Either GlobError String
globToRegex' ""             = Right ""
globToRegex' ('*':cs)       = case globToRegex' cs of
                                Left  e -> Left e
                                Right r -> Right $ ".*" ++ r
globToRegex' ('?':cs)       = case globToRegex' cs of
                                Left  e -> Left e
                                Right r -> Right $ '.' : r
globToRegex' ('[':'!':c:cs) = case charClass cs of
                                Left  e -> Left e
                                Right r -> Right $ "[^" ++ c : r
globToRegex' ('[':c:cs)     = case charClass cs of
                                Left  e -> Left e
                                Right r -> Right $ '[' : c : r
globToRegex' ('[':_)        = Left "unterminated character class"
globToRegex' (c:cs)         = case globToRegex' cs of
                                Left  e -> Left e
                                Right r -> Right $ escape c ++ r

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = case globToRegex' cs of
                       Left  e -> Left e
                       Right r -> Right $ ']' : r
charClass (c:cs)   = case charClass cs of
                       Left  e -> Left e
                       Right r -> Right $ c : r
charClass ""       = Left "unterminated character class"
