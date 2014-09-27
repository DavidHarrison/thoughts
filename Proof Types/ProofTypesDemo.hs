-- file: ProofTypeDemo.hs
-- Numeric instances for the types representing abstract parity and sign

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

import Control.Applicative (liftA, liftA2, pure, (<$>), (<*>))

import PropList (HasProp, PropList(..), getProp, setProp)

data UnsafeParity = Even | Odd deriving (Show,Eq)
type Parity = Either String UnsafeParity
data Par

data UnsafeSign = Pos | Zero | Neg deriving (Eq)
type Sign = Either String UnsafeSign
data Sig

signEvenErr = "Cannot deduce sign for an even number"
-- (0 -> Even (0), 2 -> Odd (1), -2 -> Odd (-1))
signAddErr = "Cannot add negative and positive"

instance Num UnsafeParity where
    a + b = case (a,b) of
        (Even,Even) -> Even
        (Odd,Odd)   -> Even
        _           -> Odd
    a * b = case (a,b) of
        (Even,_) -> Even
        (_,Even) -> Even
        _        -> Odd
    abs = id
    signum a = case a of
        Odd  -> Odd
        Even -> error signEvenErr
    fromInteger a
        | a `mod` 2 == 0 = Even
        | otherwise  = Odd
    negate = id

instance Num (Either String UnsafeParity) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = liftA abs
    signum (Right Even) = Left signEvenErr
    signum a = a
    fromInteger = pure . fromInteger
    negate = liftA negate

instance Show UnsafeSign where
    show a = case a of
        Pos  -> "Positive"
        Zero -> "Zero"
        Neg  -> "Negative"

instance Num UnsafeSign where
    a + b = case (a,b) of
        (Pos,Pos)   -> Pos
        (Pos,Zero)  -> Pos
        (Pos,Neg)   -> undefined
        (Zero,Zero) -> Zero
        (Zero,Neg)  -> Neg
        (Neg,Neg)   -> Neg
        _           -> b + a
    a * b = case (a,b) of
        (Pos,Pos) -> Pos
        (Pos,Neg) -> Neg
        (Zero,_)  -> Zero
        (Neg,Neg) -> Pos
        _         -> b * a
    abs a = case a of
        Zero -> Zero
        _    -> Pos
    signum = id
    fromInteger a
        | a > 0     = Pos
        | a == 0    = Zero
        | otherwise = Neg
    negate a = case a of
        Pos  -> Neg
        Zero -> Zero
        Neg  -> Pos

instance Num (Either String UnsafeSign) where
    (Right Pos) + (Right Neg) = Left signAddErr
    (Right Neg) + (Right Pos) = Left signAddErr
    a + b = (+) <$> a <*> b
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger
    negate = liftA negate

instance ( Num a, Num b
         , HasProp Parity (PropList a b)
         , HasProp Sign   (PropList a b)
         ) => Num (PropList a b) where
    (PropList a b) + (PropList c d) = PropList (a + c) (b + d)
    (PropList a b) * (PropList c d) = PropList (a * c) (b * d)
    abs (PropList a b) = PropList (abs a) (abs b)
    signum a@(PropList b c) = case ( (getProp a :: Either String UnsafeParity)
                                   , (getProp a :: Either String UnsafeSign)
                                   ) of
        (Right Even,Right Pos)  -> setProp nOdd
                                   $ PropList (signum b) (signum c)
        (Right Even,Right Zero) -> setProp nEven
                                   $ PropList (signum b) (signum c)
        (Right Even,Right Neg)  -> setProp nOdd
                                   $ PropList (signum b) (signum c)
        -- TODO, see if it can use generalized behavior defined below
        _           ->               PropList (signum b) (signum c)
    fromInteger n = PropList (fromInteger n) (fromInteger n)
    negate (PropList a b) = PropList (negate a) (negate b)

{-
instance (Num a, Num b) => Num (PropList a b) where
    (PropList a b) + (PropList c d) = PropList (a + c) (b + d)
    (PropList a b) * (PropList c d) = PropList (a * c) (b * d)
    abs    (PropList a b) = PropList (abs a) (abs b)
    signum (PropList a b) = PropList (signum a) (signum b)
    fromInteger a         = PropList (fromInteger a) (fromInteger a)
    negate (PropList a b) = PropList (negate a) (negate b)
-}

nEven :: Parity
nEven = pure Even
nOdd :: Parity
nOdd = pure Odd

nPos :: Sign
nPos = pure Pos
nZero :: Sign
nZero = pure Zero
nNeg :: Sign
nNeg = pure Neg
