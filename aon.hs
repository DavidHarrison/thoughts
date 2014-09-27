import Prelude ()

s, (<*>) :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)
(<*>) = s

k, const :: a -> b -> a
k x y = x
const = k

i, id :: a -> a
i = s k k
id = i

b, (.), (<$>) :: (b -> c) -> (a -> b) -> a -> c
b = s (k s) k
(.) = b
(<$>) = b

c, flip :: (a -> b -> c) -> b -> a -> c
c = s (b b s) (k k)

true, false :: a -> a -> a
true  = k
false = k i

and, or :: (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a)
and = (\f g x y -> f (g x y) y)
or  = (\f g x y -> f x (g x y))

not :: (a -> a -> a) -> (a -> a -> a)
not = c

((,)) :: a -> b -> (a -> b -> c) -> c
