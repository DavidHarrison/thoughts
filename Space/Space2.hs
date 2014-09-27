-- file: Space2.hs

class Space a where
    onDim :: (a -> 

data Point a = Point a

instance Space (Point a) where
    back  = id
    forth = id

data Layer a = Layer [a] a [a]

instance Space a
