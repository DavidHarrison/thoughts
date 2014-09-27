-- file: SI.hs
-- data type for SI units

{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate, (\\))

class ShowLatex a where
    showLatex :: IsString b => a -> b

data SIAtom = Inverse SIAtom
            | Unitless
            | Metre
            | Gram
            | Second
            | Ampere
            | Kelvin
            | Candela
            | Mole

instance Show SIAtom where
    show unit = case unit of
        Inverse a -> show a ++ "-1"
        Unitless  -> "1"
        Metre     -> "m"
        Gram      -> "g"
        Second    -> "s"
        Ampere    -> "A"
        Kelvin    -> "K"
        Candela   -> "cd"
        Mole      -> "mol"

instance ShowLatex SIAtom where
    show unit = case unit of
        Inverse a -> "\\per" ++ showLatex a
        Unitless  -> ""
        Metre     -> "\\metre"
        Gram      -> "\\gram"
        Second    -> "\\second"
        Ampere    -> "\\ampere"
        Kelvin    -> "\\kelvin"
        Candela   -> "\\candela"
        Mole      -> "\\mole"

instance Eq SIAtom where
    Unitless == Unitless = True
    Metre    == Metre    = True
    Gram     == Gram     = True
    Second   == Second   = True
    Ampere   == Ampere   = True
    Kelvin   == Kelvin   = True
    Candela  == Candela  = True
    Mole     == Mole     = True
    a        == b        = False
    
data SIUnit = Atom SIAtom
            | Product SIUnit SIUnit

instance Show SIUnit where
    show u = case u of
        Atom     a  -> show a
        Product  as -> "(" ++ (intercalate " * " $ map show as) ++ ")"

instance Eq SIUnit where
    Atom a == Atom b = a == b
    Product a b == Product c d
        | complement (complement a b) d == Unitless = True
        | otherwise = False
    a == b = False

complement :: SIUnit -> SIUnit -> SIUnit
complement (SIAtom a) (SIAtom b)
    | a == b    = Unitless
    | otherwise = a
complement (SIAtom a) (Product b c)
    | complement a b == Unitless = Unitless
    | complement a c == Unitless = Unitless
    | otherwise                  = a
complement (Product a b) c = case (complement a c, complement b c) of
    (Unitless, x) -> x
    (x, Unitless) -> x
    (x, y)        -> Product x y

complements :: SIUnit -> SIUnit -> (SIUnit,SIUnit)
complements a b = (complement a b, complement b a)

invert :: SIUnit -> SIUnit
invert (Atom    a  ) = Atom $ simplify $ Inverse a
invert (Product a b) = Product (invert a) (invert b)
    
simplify :: SIUnit -> SIUnit
simplify x = case x of
    Atom a -> case a of
        Inverse (Inverse b) -> simplify b
        _                   -> a
    Product a b -> if complement (invert a) b == Unitless
                   then Unitless
                   else (uncurry Product) (complements a b)

data SI a = SI { num :: a, unit :: SIUnit }
    deriving (Eq)

instance Show a => Show (SI a) where
    show (SI n u) = show n ++ " " ++ show u

{-
instance Num a => Num (SI a) where
    (SI na ua) + (SI nb ub)
        | ua == ub  = SI (na + nb) ua
        | otherwise = error $ "Incompatible units for summation: "
                              ++ show ua ++ ", " ++ show ub
    (SI na ua) * (SI nb ub) = SI (na * nb) $ simplify $ Product ua ub
    abs (SI n u)    = SI (abs n) u
    signum (SI n u) = SI (signum n) u
    fromInteger i   = SI (fromInteger i) Unitless
    negate (SI n u) = SI (negate n) u

instance Fractional a => Fractional (SI a) where
    fromRational r = SI (fromRational r) Unitless
    recip (SI n u) = SI (recip n) $ simplify $ Quotient Unitless u
-}
