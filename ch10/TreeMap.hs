module TreeMap where

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
            deriving (Show)

treeLengths :: Tree [a] -> Tree Int
treeLengths (Leaf xs)  = Leaf $ length xs
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)   = Leaf $ f x
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

--Already defined in GHC.Base
--class Functor f where
--    fmap :: (a -> b) -> f a -> f b
--
--instance Functor [] where
--    fmap = map
--
--in Data.Maybe
--instance Functor Maybe where
--    fmap _ Nothing  = Nothing
--    fmap f (Just x) = Just (f x)

instance Functor Tree where
    fmap = treeMap
