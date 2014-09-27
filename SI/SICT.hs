-- file: SICT.hs
-- Compile-time SI unit checking

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

import Control.Applicative (Applicative, pure, liftA, liftA2, (<*>))

data Unitless
data Metre
data Gram
data Second
data Ampere
data Kelvin
data Candela
data Mole

data Inverse a
data Product a b

class SIUnit a
instance SIUnit Unitless
instance Show Unitless where
    show = const "1"
instance SIUnit Metre
instance Show Metre where
    show = const "m"
instance SIUnit Gram
instance Show Gram where
    show = const "g"
instance SIUnit Second
instance Show Second where
    show = const "s"
instance SIUnit Ampere
instance Show Ampere where
    show = const "A"
instance SIUnit Kelvin
instance Show Kelvin where
    show = const "K"
instance SIUnit Candela
instance Show Candela where
    show = const "cd"
instance SIUnit Mole
instance Show Mole where
    show = const "mol"
instance (SIUnit a) => SIUnit (Inverse a)
instance Show a => Show (Inverse a) where
    show = const $ "(" ++ show (undefined :: a) ++ ")" ++ "-1"
instance (SIUnit a, SIUnit b) => SIUnit (Product a b)
instance (Show a, Show b) => Show (Product a b) where
    show = const $ show (undefined :: a) ++ " * " ++ show (undefined :: b)

class (SIUnit a, SIUnit b) => Same a b
instance Same Unitless Unitless
instance Same Metre    Metre
instance Same Gram     Gram
instance Same Second   Second
instance Same Ampere   Ampere
instance Same Kelvin   Kelvin
instance Same Candela  Candela
instance Same Mole     Mole
instance (Same a b) => Same (Inverse a) (Inverse b)
instance (Same a b) => Same a (Inverse (Inverse b))
instance (Same a b) => Same (Inverse (Inverse a)) b
instance Subset2 a b c => Same a (Product b c)
instance Subset2 c a b => Same (Product a b) c

class (SIUnit a, SIUnit b, SIUnit c) => Subset2 a b c
instance Elem a b => Subset2 a b c
instance Elem a c => Subset2 a b c
instance (Subset2 a c d, Subset2 b c d) => Subset2 (Product a b) c d

class SIUnit b => Elem a b
instance Same a b => Elem a b
instance (Elem a b) => Elem a (Product b c)
instance (Elem a c) => Elem a (Product b c)

data SI a b = SI { num :: a }

instance (Show a, SIUnit b, Show b) => Show (SI a b) where
    show (SI x) = show x ++ " " ++ show (undefined :: b)

{-
instance Functor (SI a) where
    fmap = liftA

instance Applicative (SI a) where
    pure x = SI x
    (SI f) <*> (SI x) = SI $ f x
-}

siAdd :: (Num a, SIUnit b) => SI a b -> SI a b -> SI a b
siAdd (SI a) (SI b) = SI $ a + b

siTimes :: (Num a, SIUnit b, SIUnit c)
        => SI a b -> SI a c -> SI a (Product b c)
siTimes (SI a) (SI b) = SI $ a * b
