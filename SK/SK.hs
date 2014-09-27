-- file: SK.hs
-- SK Combinator Definitions in Haskell

{-# LANGUAGE GADTs #-}

data Term x where
    K     :: Term (a -> b -> a)
    S     :: Term ((a -> b -> c) -> (a -> b) -> a -> c)
    Const :: a -> Term a
    (:@)  :: Term (a -> b) -> Term a -> Term b
infixl 6 :@

instance Show a => Show (Term a) where
    show K         = "K"
    show S         = "S"
    show (Const x) = show x
    show (a :@ b)  = "a" ++ " " ++ "b"

eval :: Term a -> Term a
eval (K :@ x :@ y)      = x
eval (S :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval x                  = x

i :: Term a -> Term a
i a = S :@ K :@ undefined :@ a
