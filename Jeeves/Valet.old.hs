-- file: Valet.hs
-- Haskell implementation of Jeeves inspired values

module Valet
( Permission
, Valet
, mkValet
, concrete
, unsafePermissions
, unsafeValues
) where

import Control.Arrow       ((***))
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad       (liftM, ap)
import Data.Monoid         (Monoid(..))

type Permission a = a -> Bool
data Valet a b    = Valet {content :: [(Permission a,b)]}

-- for testing purposes only
instance Show b => Show (Valet a b) where
  show (Valet xs) = show $ map snd xs

-- for testing purposes only
instance Eq b => Eq (Valet a b) where
  (Valet xs) == (Valet ys) = and $ zipWith (==) (snd $ unzip xs) (snd $ unzip ys)

instance Functor (Valet a) where
  fmap = liftM

instance Applicative (Valet a) where
  pure  = return
  (<*>) = ap

instance Monoid (Valet a b) where
  -- mempty :: Valet a
  mempty                        = Valet []
  -- mappend :: Valet a -> Valet a -> Valet a
  mappend (Valet xs) (Valet ys) = Valet $ xs ++ ys

instance Monad (Valet a) where
  -- return x :: a -> Valet a
  return x = Valet [(const True,x)]
  -- (>>=) :: Valet a -> (a -> Valet b) -> Valet b
  (Valet xs) >>= f = mconcat $ map (applyInScope f) xs
    where
      applyInScope f (p,v) = Valet $ map (combine p *** id) $ content $ f v
      combine a b = (\u -> a u && b u)

-- mkValet: safely construct a Valet, with a list of permissions and values
--          and a default value
-- c: content; list of Permission-value pairs
-- dv: default value
mkValet :: [(Permission a,b)] -> b -> Valet a b
mkValet c dv = Valet $ c ++ [(const True, dv)]

-- concrete: give the value corresponding to the given context from the given
--           Valet
-- c: context
-- xs: list of Permission-value pairs
concrete :: a -> Valet a b -> b
concrete c (Valet xs) = snd $ head $ filter fst $ map (($ c) *** id) xs

unsafePermissions :: (Valet a b) -> [Permission a]
unsafePermissions (Valet xs) = fst $ unzip xs

unsafeValues :: (Valet a b) -> [b]
unsafeValues (Valet xs) = snd $ unzip xs
