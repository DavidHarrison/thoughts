subsetSum :: (Num a, Eq a) => a -> [a] -> Bool
subsetSum s (a:as)
    | s == (-a) = True
    | otherwise = (subsetSum s as) || (subsetSum (s + a) as)
subsetSum 0 [] = True
subsetSum _ _  = False
