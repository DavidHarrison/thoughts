-- file: FuzzyIf.hs
-- if for fuzzy truth values, useful for non-determinate proof types
-- (e.g. checking properties outside of the scope of the types specification)

class FBool a where
    absolutely :: Bool -> FBool -> Bool

-- fuzzy boolean
data FB a = T a | F a | Q a
instance Functor FB where
    fmap f (T a) = T $ f a
    fmap f (F a) = F $ f a
    fmap f (Q a) = Q $ f a
instance Applicative FB where
    pure a = T a
    a <*> b = case (a,b) of
                   (T f,T x) = T $ f x
                   (F f,F x) = F $ f x
                   _         = Q $ f x

instance FBool FB where
    absolutely a b = case (a,b) of
                          (True, T) -> True
                          (False,F) -> True
                          _         -> False

instance FBool Bool where
    absolutely True  True  = True
    absolutely False False = True
    absolutely _     _     = False

data Probability = { p :: Double, sd :: Double }
instance Probability

class Fuzzy a where

-- fuzzy if
fif :: (FBool b, Fuzzy f) => b -> a -> a -> f a
fif p a b
    | absolutely True  p = pure a
    | absolutely False p = pure b
    | otherwise          =

-- tell whether the FBool is always the given Bool
absolutely :: Bool -> FBool -> Bool
