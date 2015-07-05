module ValidFunctor where

data Foo a = Foo a

instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
