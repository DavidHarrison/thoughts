-- file: Valet.hs
-- Haskell implementation of Jeeves inspired values

module Valet
  ( Valet
  , valet
  , concrete
  ) where

import Control.Applicative (Applicative(pure,(<*>)))
import Control.Monad       (liftM, ap)

data Valet a b
  = Node (a -> Bool) (Valet a b) (Valet a b)  -- ^ Label High Low
  | Leaf b                                    -- ^ Unlabeled value.

instance Functor (Valet a) where
  fmap = liftM

instance Applicative (Valet a) where
  pure  = return
  (<*>) = ap

instance Monad (Valet a) where
  -- return x :: a -> Valet a b
  return v = Leaf v
  -- (>>=) :: Valet a b -> (b -> Valet a c) -> Valet a c
  (Node p h l) >>= f = Node p (h >>= f) (l >>= f)
  (Leaf v)     >>= f = f v

-- | Safely construct a Valet.
valet :: (a -> Bool)  -- ^ Label checking context
        -> b          -- ^ High security value
        -> b          -- ^ Low security value
        -> Valet a b  -- ^ Valet
valet p h l = Node p (Leaf h) (Leaf l)

-- | Give the concrete value from the given Valet and context.
concrete :: a          -- ^ Context
         -> Valet a b  -- ^ Valet
         -> b          -- ^ Concrete value
concrete c (Node p h l)
  | p c       = concrete c h
  | otherwise = concrete c l
concrete _ (Leaf v) = v
