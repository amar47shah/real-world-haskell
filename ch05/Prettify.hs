module Prettify
  (
    Doc
  , text
  , double
  , char
  , (<>)
  , hcat
  ) where

data Doc = ToBeDefined
         deriving (Show)

text :: String -> Doc
text   str = undefined

double :: Double -> Doc
double num = undefined

char :: Char -> Doc
char c = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined
