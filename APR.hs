-- file: APR.hs
-- arbitrary precision rationals
-- use compile time types to optimize operations on rationals to an arbitrary
-- degree of precision

{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes #-}

import Control.Applicative (Applicative, pure, (<*>), liftA, liftA2)

class Nat a where
    value :: a -> Int 
data Zero
data Succ a

instance Nat Zero where
    value _ = 0
instance Nat a => Nat (Succ a) where
    value _ = 1 + value (undefined :: a)

toNat :: Nat a => Int -> a
toNat 0 = (undefined :: Zero)
toNat n = succOf $ toNat $ n - 1

succOf :: Nat a => a -> Succ a
succOf _ = undefined

data APR a = (forall a. Nat a, forall b. Floating b) => APR b

instance Functor (APR a)
instance Applicative APR

instance Num b => Num (APR a b) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum
    fromInteger = (,) undefined . fromInteger
    negate = liftA negate

instance Fractional b => Fractional (APR a b) where
    recip = liftA recip
    fromRational = (,) undefined . fromRational

instance Floating b => Floating (APR a b) where
    pi = (undefined,pi)
    -- can be implemented using Taylor Series
    exp = liftA exp
    log = liftA log
    sin = liftA sin
    cos = liftA cos
    asin = liftA asin
    atan = liftA atan
    acos = liftA acos
    sinh = liftA sinh
    cosh = liftA cosh
    asinh = liftA asinh
    atanh = liftA atanh
    acosh = liftA acosh
