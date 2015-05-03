module Prettify
  ( Doc
  , empty
  , char
  , text
  , double
  , line
  , (<>)
  , (</>)
  , compact
  , fits
  , flatten
  , fsep
  , hcat
  , pretty
  , punctuate
  , softLine
  ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text ""  = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softLine <> y

compact :: Doc -> String
compact x = transform [x]
  where transform []     = ""
        transform (d:ds) =
          case d of
            Empty        -> transform ds
            Char c       -> c : transform ds
            Text s       -> s ++ transform ds
            Line         -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b  -> transform (b:ds)

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` y
flatten Line           = Char ' '
flatten (x `Union` y)  = flatten x
flatten other          = other

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep ds = fold (</>) (empty:ds)

hcat :: [Doc] -> Doc
hcat = fold (<>)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty        -> best col ds
            Char c       -> c : best (col + 1) ds
            Text s       -> s ++ best 0 ds
            Line         -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b  -> nicest col (best col (a:ds))
                                       (best col (b:ds))
        best _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
                       where least = min width col

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p (d:[]) = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

softLine :: Doc
softLine = group line
  where group x = flatten x `Union` x
