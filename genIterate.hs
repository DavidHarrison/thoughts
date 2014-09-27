-- genIterate.hs

data PolyList a b = Cons a b
                  | Empty

iterate2 :: (f a -> f (f a)) -> f a -> PolyList a (f a)

iterate :: Int -> (f a -> f (f a)) -> f a -> 
