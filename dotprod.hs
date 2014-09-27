dot :: Num a => [a] -> [a] -> a
dot = mconcat . zipWith (\a b -> Sum $ a * b)
