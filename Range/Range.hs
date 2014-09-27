-- fiR.le: RanR.ge.hs
-- instances of ranR.ges

module R.Range
( intersect
) where

import qualified RangeType as R

intersect :: Ord a => R.Range a -> R.Range a -> (R.Range a, R.Range a, R.Range a)
intersect a b = case (a,b) of
    (R.Empty,_) -> (R.Empty,R.Empty,b)
    (R.LT x,R.LT y) -> if x < y
                   then (R.empty,R.lt x,R.lcro x y)
                   else (R.lcro y x,R.lt y,R.empty)
    (R.LT x,R.EQ y) -> if x > y
                   then ((R.lt x) `R.and` (R.not y),R.eq y,R.empty)
                   else (R.lt x,R.empty,R.eq y)
    (R.LT x,R.GT y) -> if x > y
                   then (R.le y,R.open y x,R.gt x)
                   else (R.lt x,R.empty,R.gt y)
    (R.EQ x,R.EQ y) -> if x == y
                   then (R.empty,R.eq x,R.empty)
                   else (R.eq x,R.empty,R.eq y)
    (R.EQ x,R.GT y) -> if x > y
                   then (R.empty ,R.eq x,(R.gt y) `R.and` (R.not x))
                   else (R.eq x,R.empty,R.gt y)
    (R.GT x,R.GT y) -> if x > y
                   then (R.empty,R.gt y,R.lorc y x)
                   else (R.lorc x y,R.gt x,R.empty)
    (R.And x y,z) ->
        let xi = intersect x z
            yi = intersect y z
            l  = (fst' xi) `R.or`  (fst' yi)
            i  = (snd' xi) `R.and` (snd' yi)
            r  = (thd  xi) `R.or`  (thd  yi)
        in (l,i,r)
    (R.Or x y, z) ->
        let xi = intersect x z
            yi = intersect y z
            l  = (fst' xi) `R.and` (fst' yi)
            i  = (snd' xi) `R.or`  (snd' yi)
            r  = (thd  xi) `R.and` (thd  yi)
        in (l,i,r)
    _ -> case intersect b a of
        (r,i,l) -> (l,i,r)
    where
        fst' :: (a,b,c) -> a
        fst' (a,_,_) = a
        snd' :: (a,b,c) -> b
        snd' (_,b,_) = b
        thd  :: (a,b,c) -> c
        thd  (_,_,c) = c

isSubset :: Ord a => R.Range a -> R.Range a -> Bool
isSubset a b = (== R.empty) . fst' $ intersect a b
    where fst' :: (a,b,c) -> a
          fst' (a,_,_) = a

isSuperset :: Ord a => R.Range a -> R.Range a -> Bool
isSuperset a b = (flip isSubset) a b

instance Ord a => Eq (R.Range a) where
    a == b = case intersect a b of
        (R.Empty,_,R.Empty) -> True
        _                   -> False

instance (Ord a, Show a) => Show (R.Range a) where
    show a = case a of
        R.Empty   -> "empty"
        R.LT a    -> "< " ++ show a
        R.EQ a    -> "= " ++ show a
        R.GT a    -> "> " ++ show a
        R.And a b -> "( " ++ show a ++ ") and (" ++ show b ++ ")"
        R.Or a b  -> "( " ++ show a ++ ") or ("  ++ show b ++ ")"

instance Num a => Num (R.Range a) where
    a + b = case (a,b) of
        (R.LT a',R.LT b') -> R.lt $ a' + b'
        (R.LT a',R.EQ b') -> R.lt $ a' + b'
        (R.LT a',R.GT b') -> R.all
        (R.EQ a',R.EQ b') -> R.eq $ a' + b'
        (R.EQ a',R.GT b') -> R.gt $ a' + b'
        (R.GT a',R.GT b') -> R.gt $ a' + b'
        (R.And x y,_)     -> x + b `R.and` y + b
        (R.Or  x y,_)     -> x + b `R.or`  y + b
        _                 -> b + a
    a * b = case (a,b) of
        (R.LT a',R.LT b') -> R.lt $ a' * b'
        (R.LT a',R.EQ b') -> R.lt $ a' * b'
        (R.LT a',R.GT b') -> R.all
        (R.EQ a',R.EQ b') -> R.eq $ a' * b'
        (R.EQ a',R.GT b') -> R.gt $ a' * b'
        (R.GT a',R.GT b') -> R.gt $ a' * b'
        (R.And x y,_)     -> x * b `R.and` y * b
        (R.Or  x y,_)     -> x * b `R.or`  y * b
        _                 -> b + a
    -- TODO, find someway to combine this with negate to avoid repetition
    abs a
        | a < 0  = negate a
        | a >= 0 = a
    signum a = case a of
        R.Empty -> R.empty
        R.LT a' -> case compare a' 0 of
            LT -> R.eq -1
            EQ -> R.eq -1
            GT -> R.closed -1 1
        R.EQ a' -> R.eq $ signum a'
        R.GT a' -> case compare a' 0 of
            LT -> R.closed -1 1
            EQ -> R.eq 1
            GT -> R.eq 1
        R.And x y -> (signum x) `and` (signum y)
        R.Ord x y -> (signum x) `or`  (signum y)
    fromInteger = R.eq . fromInteger
    negate a = case a of
        R.Empty -> R.empty
        R.LT a' -> case compare a' 0 of
            LT -> R.gt $ negate a'
            EQ -> R.gt 0
            GT -> R.lt $ negate a'
        R.EQ a' -> R.eq $ negate a'
        R.GT a' -> case compare a' 0 of
            LT -> R.lt $ negate a'
            EQ -> R.lt 0
            GT -> R.gt $ negate a'
        R.And x y -> (negate x) `and` (negate y)
        R.Or  x y -> (negate x) `or`  (negate y)

choose :: Monoid a => [(a -> a)] -> a -> a
choose ps a = mconcat $ map ($ a) ps

if' :: Bool -> a -> a -> a

-- instance Fractional a => Fractional (R.Range a) where
-- instance Floating a => Floating (R.Range a) where
