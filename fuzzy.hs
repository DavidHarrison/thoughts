-- file: fuzzy.hs

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

import Prelude             hiding ( (&&), (||), not )
import Control.Applicative        ( Applicative, liftA, liftA2, pure, (<*>)
                                  , (<$>)
                                  )
import Control.Monad              ( ap, liftM )
import Data.Function              ( on )
import Data.List                  ( groupBy, intercalate, intersect, union )
import Data.Monoid                ( Monoid, First(First), mappend, mconcat
                                  , mempty
                                  )
import Data.Tuple                 ( swap )

class Monoiding m

class Membership a where
    type Fuzzed a b :: *
    (&&)  :: a -> a -> a
    (||)  :: a -> a -> a
    not   :: a -> a
    true  :: a
    false :: a
    fuzz  :: b -> a -> Fuzzed a b

this, however :: (Membership a, Monoid b) => (a -> b) -> (a -> b) -> a -> b
this a {- else -} b {- if -} = mappend <$> a <*> b . not
however {- a -} {- b if c -} = flip this

if' :: (Membership a, Monoid b) => a -> (a -> b) -> (a -> b) -> b
if' c a b = this a b c

fromBool :: Membership a => Bool -> a
fromBool True  = true
fromBool False = false

instance Membership Bool where
    type Fuzzed Bool a = First a
    (&&) = max
    (||) = min
    not True  = False
    not False = True
    true  = True
    false = False
    fuzz a True  = First $ Just a
    fuzz _ False = First Nothing

type Probability = Float

data Prob a = Prob { runProb :: [(Probability,a)] }

instance Show a => Show (Prob a) where
    show = intercalate "\n" . map show' . runProb
      where show' (p,v) = show v ++ " (" ++ show (p * 100) ++ "% chance)"

instance Monoid (Prob a) where
    mempty = Prob []
    (Prob vs) `mappend` (Prob vs') = Prob $ filter ((/= 0) . fst) (vs ++ vs')

instance Functor Prob where
    fmap = liftM

instance Applicative Prob where
    pure = return
    (<*>) = ap

instance Monad Prob where
    return = Prob . return . (,) 1
    (Prob vs) >>= f = mconcat $ vs >>= return . (\(p,v) -> weight p $ f v)
      where
          weight :: Probability -> Prob a -> Prob a
          weight p = Prob . map (\(a,b) -> (p * a, b)) . runProb

condense :: Eq a => Prob a -> Prob a
condense = Prob . map (\ps -> (sum (map fst ps),snd (head ps)))
         . groupBy ((==) `on` snd) . runProb

instance Membership Float where
    type Fuzzed Float a = Prob a
    (&&) = max
    (||) = min
    not  = (-) 1
    true  = 1
    false = 0
    fuzz a p = Prob [(p,a)]

{-
class Set s where
    intersect :: s a -> s a -> s a
    union     :: s a -> s a -> s a

instance Set s => Membership (s a,s a) where
    type Fuzzed ([a],[a]) = [a]
    (&&) (a,b) (b,d) = (a `intersect` c, b `union` d)
    (||) (a,b) (b,d) = (a `union` c, b `intersect` d)
    not   = swap
    true  = (true,false)
    false = (false,true)
    fuzz  = flip (,) $ []

class Predicate p where
    member :: forall a b. Membership b => p a b -> a -> b

instance Predicate (->) where
    member = ($)

data Set a b = forall a b. Eq a => Set { predicate :: a -> b }

instance Functor (Set a) where
    fmap = liftM

instance Applicative (Set a) where
    pure = return
    (<*>) = ap

instance Monad (Set a) where
    return = Set . return
    (Set a) >>= f = Set $ a >>= predicate . f

instance (Eq a, Membership b) => Membership (Set a b) where
    type Fuzzed (Set a b) c = (Set c b)
    (&&) = liftA2 (&&)
    (||) = liftA2 (||)
    not  = liftA  not
    true  = Set $ const true
    false = Set $ const false
    -- fuzz  :: c -> (a -> b) -> (c -> b)
    fuzz v a = Set $ (\b -> fromBool $ v == b)
-}

data Parity = Even | Odd
data Connective a = And (Connective a) (Connective a)
                  | Or  (Connective a) (Connective a)
                  | Not (Connective a)
                  | Atom a

instance Functor Connective where
    fmap f (And  a b) = And  (f a) (f b)
    fmap f (Or   a b) = Or   (f a) (f b)
    fmap f (Not  a  ) = Not  (f a)
    fmap f (Atom a  ) = Atom (f a)

instance Applicative Connective where
    pure = Atom
    (And f g) <*> c = fmap f c `And` fmap g c
    (Or  f g) <*> c = fmap f c `Or`  fmap g c
    (Not f  ) <*> c = Not $ fmap f c

instance Membership (Connective Parity) where
    (Atom Even) && (Atom Even) = Even
    (Atom Odd ) && (Atom Odd ) = Odd
    (Atom _   ) && (Atom _   ) = And Even Odd
    (And a b )  && (And a b  )

{-
instance Membership m => Membership (a -> m) where
    type Fuzzed (a -> m) a = (a -> m)
    (&&)  = liftA2 (&&)
    (||)  = liftA2 (||)
    not   = liftA  not
    true  = const  true
    false = const  false
    fuzz  = const . (==)

f :: (a -> b) -> [a] -> [b]
f :: (a -> b) -> (a -> m) -> (b -> m)
    
 - Membership m => s a = (a -> m)
 - if :: (s a -> s a) -> (s a -> s b) -> (s a -> s b) -> (s a -> s b)
 - if :: ((a -> m) -> (a -> m)) -> ((a -> m) -> (b -> m)) -> ((a -> m) -> (b -> m)) -> (a -> m) -> (b -> m)
 - f :: (a -> b) -> (a -> m) -> (b -> m)
 - f = 
 - (&&), (||) :: s a -> s a -> s a
 - (&&), (||) :: (a -> m) -> (a -> m) -> (a -> m)
 - not :: s a -> s a
 - not :: (a -> m) -> (a -> m)
 - true, false :: s a
 - true, false :: (a -> m)
 - fuzz :: b -> s a -> s b
 - fuzz :: b -> (a -> m) -> (b -> m)
 -}

{-
data Subset a b = Subset { subset :: a -> b, superset :: a -> b }

instance (Eq a, Membership b) => Membership (Subset a b) where
    type Fuzzed (Subset a b) c = Subset b c
    (Subset a b) && (Subset c d) = Subset (a && c) (b && d)
    (Subset a b) || (Subset c d) = Subset (a || c) (b || d)
    not (Subset a b) = Subset (not a) b
    true  = Subset true true
    false = Subset false false
    fuzz a s = Subset (return a) (return a)
-}
