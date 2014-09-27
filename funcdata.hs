-- file: funcdata.hs
-- functional data representations

{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Applicative (Applicative, (<*>), pure)
import Data.Foldable       (Foldable, foldMap)
import Data.Monoid         ((<>), mempty)

type (.*.) a b = forall c. c => (a -> b -> c) -> c
type (.+.) a b = forall c. c => (a -> c) -> (b -> c) -> c

prod :: a -> b -> a .*. b
prod = (\a b f -> f a b)

sumA :: a -> a .+. b
sumA = (\a f g -> f a)
sumB :: b -> a .+. b
sumB = (\b f g -> g b)

newtype Either' a b = Either' (a .+. b)

left :: a -> Either' a b
left  = Either' . sumA
right :: b -> Either' a b
right = Either' . sumB

instance Functor (Either' a b) where
    fmap f (Either' s) = s left (right . f)

instance Applicative (Either' a b) where
    pure = right
    (Either' a) <*> (Either' b) = a left (\f -> fmap f b)

instance Monad (Either' a b) where
    return = pure
    (Either' x) >>= f = x left f

data List a = List (Null .+. (a .*. (List a)))

nul :: a .*. b
nul = (\f -> nul)

head :: List a -> a
head (List l) = l (\a _ -> a)

tail :: List a -> List a
tail (List l) = l (\_ b -> b)

instance Functor (List a) where
    fmap f (List l) = l (\a b -> List $ prod (f a) (fmap f b))

instance Foldable (List a) where
    foldMap f l = foldMap' f mempty l
      where foldMap' f z (List l) = l (\a b -> foldMap' f (z <> f a) b)
