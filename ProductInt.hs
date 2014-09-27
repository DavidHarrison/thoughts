type Int' = [Bool]

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

plus :: Int' -> Int' -> Int'
plus = plus' False
  where
      plus' :: (Bool,Int') -> Int' -> Int' -> Int'
      plus' (i,z) (a:as) (b:bs) = plus' ( ((i && a) || (i && b) || (i && b))
                                        , ((a `xor` b `xor` c) : z)
                                        )
                                        as bs
      plus' (i,z) []     (b:bs) = plus' ((b && i), (b `xor` i)) [] bs
      plus' (i,z) (a:as) []     = plus' ((b && i), (b `xor` i)) as []
      plus' (_,z) []     []     = reverse z

times :: Int' -> Int' -> Int'
times a b = fold plus [] $ zipWith (\i b -> if b then i else []) (iterate ((:) False) a) b
  where
      times' ::

data TI = N TI TI | L Bool

instance Eq TI where
    a == b = a - b == 0

instance Num TI where
    (L False) + b = b
    a + b = N a b
    (L True ) * b = b
    (L False) * _ = L False
    (N a b) * (N c d) = (a * c) + (a * d) + (b * c) + (b * d)
