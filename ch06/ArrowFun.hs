-- Chapter 6, Exercise 1
-- Briefly, Control.Arrow.second is a function to wrap the
-- computation of a function applied to only the second element
-- of a tuple. Mapping second allows you to apply a function
-- exclusively to the second element of each tuple in a list:
-- map . second :: (b -> c) -> [(d, b)] -> [(d, c)]
-- The complementary function first behaves like this:
-- map . first  :: (b -> c) -> [(b, d)] -> [(c, d)]
--
-- As an alternative to map (second f),
--   map (\(a, b) -> (a, f b)) less abstracted, easier to grasp, version.
-- See discussion here:
-- http://stackoverflow.com/questions/10818779/can-i-map-the-first-element-of-a-pair-without-arrows
--
-- The following sample code is from the tutorial here:
-- https://wiki.haskell.org/Arrow_tutorial
{-# LANGUAGE Arrows #-}
module ArrowFun where

import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

--Existing functions
--first :: (Arrow a) => a b c -> a (b, d) (c, d)
--second :: (Arrow a) => a b c -> a (d, b) (d, c)
--(>>>) :: Category cat => cat a b -> cat b c -> cat a c

newtype SimpleFunc a b = SimpleFunc {
    runF :: (a -> b)
}

instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first (SimpleFunc f) = SimpleFunc (mapFst f)
                where mapFst g (a, b) = (g a, b)
  second (SimpleFunc f) = SimpleFunc (mapSnd f)
                where mapSnd g (a, b) = (a, g b)

instance Category SimpleFunc where
    (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
    id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x, x))

unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry

liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op

-- Try it out!
f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> 3*x + 1)

h = liftA2 (+) f g
hOutput :: Int
hOutput = runF h 8

--doesn't require split, unsplit, first, second, or liftA2
h' :: SimpleFunc Int Int
h' = proc x -> do
       fx <- f -< x --try x^2
       gx <- g -< x
       returnA -< (fx + gx)

hOutput' :: Int
hOutput' = runF h' 8
