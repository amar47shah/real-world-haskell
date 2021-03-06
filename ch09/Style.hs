tidyLet = let foo = undefined
              bar = foo * 2
          in undefined

anotherLet = let foo = undefined
                 bar = foo * 2
              in undefined

weirdLet = let foo = undefined
               bar = foo * 2
    in undefined

strangeLet = let foo = undefined
                 bar = foo * 2 in
    undefined

--
commonDo, rareDo, anotherDo :: IO ()

commonDo = do
  something <- undefined
  return ()

rareDo =
  do something <- undefined
     return ()

anotherDo = do something <- undefined
               return ()

--

unusualPunctuation =
  [ (x,y) | x <- [1..a], y <- [1..b] ] where {
                                           b = 7;
 a = 6 }

preferredLayout = [ (x,y) | x <- [1..a], y <- [1..b] ]
  where b = 7
        a = 6

normalIndent =
    undefined

strangeIndent =
                           undefined

--

goodWhere = take 5 lambdas
    where lambdas = []

alsoGood =
    take 5 lambdas
  where
    lambdas = []

badWhere =           -- legal, but ugly and hard to read
    take 5 lambdas
    where
    lambdas = []
