-- file: randomAccess.hs

data RArray = End a
            | Node { values :: [RArray a], size :: Int }

(!>) :: RArray a -> Int -> Maybe a
(End a) !> 0 = a
(End a) !> _ = undefined
(Node vs s) !> i = i `mod`

(<!) :: RArray a -> Int -> a -> RArray a
(End _) <! 0 v = (End v)
(End _) <! _ _ = undefined
(Node vs s)
