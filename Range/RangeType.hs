-- file: RangeData.hs
-- Range data type, giving a non-deterministic representation for a range of
-- ordinals

{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- should be imported qualified (overlap with Prelude)
module RangeType
( Range(..)
, empty
, lt
, eq
, gt
, and
, or
, ge
, le
, open
, closed
, lorc
, lcro
, not
, all
) where

import qualified Prelude             as P
import           Control.Applicative ((<$>),(<*>))

data Range a where
    Empty :: Range a -- possibly should be replaced with a Maybe/Either
    LT    :: P.Ord a => a -> Range a
    EQ    :: P.Ord a => a -> Range a
    GT    :: P.Ord a => a -> Range a
    And   :: Range a -> Range a -> Range a
    Or    :: Range a -> Range a -> Range a

empty :: Range a
empty = Empty

lt :: P.Ord a => a -> Range a
lt = LT

eq :: P.Ord a => a -> Range a
eq = EQ

gt :: P.Ord a => a -> Range a
gt = GT

and :: Range a -> Range a -> Range a
and = And

or :: Range a -> Range a -> Range a
or = Or

-- greater than or equal to
ge :: P.Ord a => a -> Range a
ge = or <$> gt <*> eq

-- less than or equal to
le :: P.Ord a => a -> Range a
le = or <$> lt <*> eq

-- open (left and right) interval
open :: P.Ord a => a -> a -> Range a
open a b = (gt a) `and` (lt b)

-- closed (left and right) interval
closed :: P.Ord a => a -> a -> Range a
closed a b = (ge a) `and` (le b)

-- left-open, right-closed interval
lorc :: P.Ord a => a -> a -> Range a
lorc a b = (gt a) `and` (le b)

-- left-closed, right-open interval
lcro :: P.Ord a => a -> a -> Range a
lcro a b = (ge a) `and` (lt b)

-- not equal to
not :: P.Ord a => a -> Range a
not = or <$> lt <*> gt

-- all numbers of the set
-- could add an argument to generalize away the Num constraint,
-- but that does not currently seem necessary
all :: (P.Ord a, P.Num a) => Range a
all = le 0 `or` gt 0
