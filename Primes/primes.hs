{-
 - File: primes.hs
 - calculate the nth prime
 -}

import System.Environment

main :: IO ()
main = getArgs >>= putStrLn . show . prime . read . head

prime :: Integer -> Integer
prime n = prime_iter 1 2 n

{-
 - z - running product of primes
 - t - number to test
 - n - number of primes left
 -}
prime_iter :: Integer -> Integer -> Integer -> Integer
prime_iter z t n
  | and [(n == 1),(t_is_prime)] = t
  | t == 2 = prime_iter 2 3 (n - 1)
  | t_is_prime = prime_iter (z * t) (t + 2) (n - 1)
  | otherwise = prime_iter z (t + 2) n
    where t_is_prime = is_prime z t

is_prime :: Integer -> Integer -> Bool
is_prime z t
  | t == 2 = True
  | otherwise = (gcd z t) == 1
