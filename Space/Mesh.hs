data MeshT a = MeshT [a] a [a]
data Unit  a = Unit a

data Mesh a = forall b. Containing a b => Mesh b

type OneD a   = MeshT (Unit a)
type TwoD a   = MeshT (MeshT (Unit a))
type ThreeD a = MeshT (MeshT (MeshT (Unit a)))

class Containing a b
instance Containing a (Unit a)
instance Containing a b => Containing a (MeshT b)

class Zipper z where
    next :: z a -> Maybe (z a)
    prev :: z a -> Maybe (z a)
    mapZ :: (a -> b) -> z a -> z b
    get  :: z a -> a
    dims :: z a -> Int

zero, far :: Zipper z => z a -> z a
zero = last $ catMaybes $ iterate prev
far  = last $ catMaybes $ iterate next

instance Zipper Unit where
    next = const Nothing
    prev = const Nothing
    get (Unit a) = a
    dims = 0

instance Zipper MeshT where
    next (MeshT ls c (r:rs)) = Just $ MeshT (c:ls) r rs
    next _                   = Nothing
    prev (MeshT (l:ls) c rs) = Just $ MeshT ls l (c:rs)
    prev _                   = Nothing
    mapZ (MeshT ls c rs)     = MeshT (map f ls) (f c) (map f rs)
    get  (MeshT _ c _)       = c
    dims (MeshT _ c _)       = 1 + dims c

instance Functor Unit where
    fmap f (Unit a) = Unit $ f a

instance Functor f => Functor (MeshT f) where
    fmap f (MeshT ls c rs) = MeshT (map (fmap f) ls)
                                   (fmap f c)
                                   (map (fmap f) rs)

instance Comonad (Unit a) where
    extract = get
    duplicate (Unit a) = Unit (Unit a)

instance Comonad w => Comonad (MeshT w) where
    extract = extract . get
    duplicate m = duplicate' m m
      where
          duplicate' m (MeshT ls c rs) = undefined
          shiftAt :: (Zipper a, Zipper b)
          shiftAt f a b
            | dims a <  dims b = shiftAt f a $ get b
            | dims a == dims b = f a
            | otherwise        = undefined
