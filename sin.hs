-- file: sin.hs
-- calculate the sine of a number using a power series

sin :: Floating a => a -> a
sin x = sum $ takeWhile significant $ map (term x) [0..]
  where
    term :: Floating a => a -> Integer -> a
    term x n = (-1)**n * x**(1 + 2*n) / (fac (1 + 2*n))
    significant :: Float -> Bool
    significant = (>=10**(-1*places) / 2)

fac 0 = 1
fac n = product [1..n]

places = 4
