\usepackage{hyperref}

In \hyperref[Ypnos: Declarative, Parallel Structured Grid Programming]{http://www.cl.cam.ac.uk/~dao29/publ/ypnos-damp10.pdf}, Orchard et al. propose a domain specific language in Haskell for grid-based computations. The implementation is deeply integrated with the concept of comonads. They address the possibility of adaptive mesh refinement in comonadic grid-based computations, but leave it open. I propose that refinement meshes can be made comonads by nesting grids where necessary. The dynamic nature of adaptive mesh refinement is more difficult to express in a pure comonad because it changes the shape of the grid. However, if the mesh is reducable we can map monoids to grid locations and then reconstruct the grid by combining these monoids.

Refinement meshes are often refined not only in physical dimensions, but also temporally. To allow this, locks could be placed on larger refinements while smaller ones are progressed. However, I propose that running a iterative grid-based computation can be seen as expanding and refinining the grid in a temporal direction. One major issue with this approach is that memory usage because proportional to time. To avoid this, I introduce a system for killing temporal (and other dimensional) refinements so that they may be freed in memory.

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude               hiding (zip, zipWith)
import Control.Applicative   (Applicative, pure, (<$>), (<*>))
import Control.Comonad       (Comonad, extract, duplicate, extend)
import Data.Functor.Identity (Identity(Identity))
import Data.Key              (Zip, zipWith)

class Lens l where
    -- lens :: (a -> f a) -> l a -> f (l a)
    get :: l a -> a
    -- get = getConst . lens Const
    set :: a -> l a -> l a
    set = update . const
    update :: (a -> a) -> l a -> l a
    -- update f = runIdentity . lens (Identity . f)

data Refinement m a = Fin (m a) | Ref (m (Refinement m a))
newtype SurviveT = Alive (m a) | Dead (m a)

instance (Show a, Show (m a), Show (m (Refinement m a))) => Show (Refinement m a) where
    show (Fin a) = "Fin (" ++ show a ++ ")"
    show (Ref m) = "Ref (" ++ show m ++ ")"

data View = FocL | FocR deriving (Eq,Show)
data Partition f a = Part View (f a) (f a) deriving (Eq)

instance Show (f a) => Show (Partition f a) where
    show (Part FocL l r) = "[" ++ show l ++ "] " ++ show r
    show (Part FocR l r) = show l ++ " [" ++ show r ++ "]"

switch :: Partition f a -> Partition f a
switch (Part v l r) = Part (switch' v) l r
  where switch' FocL = FocR
        switch' FocR = FocL

instance Functor f => Functor (Partition f) where
    fmap f (Part v l r) = Part v (fmap f l) (fmap f r)

instance (Comonad w, Zip w) => Comonad (Partition w) where
    extract (Part FocL l _) = extract l
    extract (Part FocR _ r) = extract r
    duplicate m@(Part v l r) = Part v l' r'
      where l' = zipWith (Part FocL) (duplicate l) (duplicate r)
            r' = zipWith (Part FocR) (duplicate l) (duplicate r)

instance Zip z => Zip (Partition z) where
    zipWith f (Part v l r) (Part v' l' r') = Part v (zipWith f l l')
                                                    (zipWith f r r')

instance Lens w => Lens (Partition w) where
    get (Part FocL l _) = get l
    get (Part FocR _ r) = get r
    update f (Part FocL l r) = Part FocL (update f l) r
    update f (Part FocR l r) = Part FocR l (update f r)

instance Lens Identity where
    get = extract
    update = fmap

instance Functor f => Functor (Refinement f) where
    fmap f (Fin w) = Fin $ fmap f w
    fmap f (Ref w) = Ref $ fmap (fmap f) w

instance (Comonad w, Lens w) => Comonad (Refinement w) where
    extract (Fin w) = extract w 
    extract (Ref w) = extract $ extract w
    duplicate (Fin w) = Fin $ fmap Fin $ duplicate w
    -- data Refinement w a = Fin a | Ref (w (Refinement w a))
    -- duplicate : Refinement w a -> Refinement w (Refinement w a)
    -- Ref : w (Refinement w a) -> Refinement w a
    duplicate (Ref w) = Ref $ f w
        where
            f :: (Comonad w, Lens w, Functor w) => w (Refinement w a) -> w (Refinement w (Refinement w a))
            f = fmap (Ref . f')
            f' :: Refinement w a -> w (Refinement w (Refinement w a))
            f' :: (Comonad w, Lens w) => Refinement w a -> Refinement w (Refinement w a)
            f' = duplicate

kill :: RefinementK m a -> RefinementK m a
kill = const Nothing
refine   :: (a -> Refinement m a) -> Refinement m a -> Refinement m a
derefine :: (Refinement m a -> a) -> Refinement m a -> Refinement m a 
manage :: Refinement m a -> Refinement m a
\end{code}
