-- primitive operations, implemented usings lambda calculus

import Prelude (undefined)

{-
id :: a -> a
id = (\x -> x)

const :: b -> a -> b
const = (\x y -> x)

flip :: (a -> b -> c) -> (b -> a -> c)
flip = (\f x y -> f y x)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (\f g x -> f (g x))

($) :: (a -> b) -> a -> b
($) = (\f x -> f x)

(:-) :: a -> (a -> b) -> b
(:-) = flip ($)

type Bool = a -> a -> a

true, false :: Bool
true = const
false = const id

if' :: Bool -> a -> a -> a
if' = ($)
-}

s :: (a -> b -> c) -> (a -> b) -> a -> c
s = (\f g x -> f x (g x))

k :: a -> b -> a
k = (\a _ -> a)

i :: a -> a
i = s k k

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = s k

($) :: (a -> b) -> a -> b
($) = i

flip :: (a -> b -> c) -> b -> a -> c
flip = (\f a b -> f b a)

(:-) :: a -> (a -> b) -> b
(:-) = flip ($)

true, false :: forall a => a -> a -> a
true  = k
false = k i

if' :: (a -> a -> a) -> a -> a -> a
if' = i

pair :: a -> b -> (a -> b -> c) -> c
pair = (\a b f -> f a b)

cons :: a -> b -> (a -> b -> c) -> c
cons = (\a b f -> f a b)
