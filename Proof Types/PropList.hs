-- file: PropList.hs
-- List of properties for proof types

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module PropList
( HasProp
, PropList(PropList)
, getProp
, setProp
, updateProp
) where

-- TODO: ensure uniqueness of properties in list and make sure first element
--       is always a non-list alternatively, expand to allow these conditions
--       to work
data PropList a b = PropList a b

class HasProp a b where
    getProp :: b -> a
    setProp :: a -> b -> b
instance HasProp a a where
    getProp = id
    setProp a = const a
instance HasProp a (PropList a b) where
    getProp   (PropList x y) = x
    setProp x (PropList y z) = PropList x z
{-
instance HasProp a b => HasProp a (PropList b c) where
    getProp   (PropList x y) = getProp x
    setProp x (PropList y z) = PropList (setProp x y) z
-}
instance HasProp a c => HasProp a (PropList b c) where
    getProp   (PropList x y) = getProp y
    setProp x (PropList y z) = PropList y (setProp x z)

updateProp :: HasProp a b => (a -> a) -> b -> b
updateProp f l = setProp (f $ getProp l) l

class Subset a b where
    subset :: b -> a
instance HasProp a b => Subset a b where
    subset = getProp
instance (Subset a c, Subset b c) => Subset (PropList a b) c where
    subset c = PropList (subset c) (subset c)

class Unifying a b where
instance (Subset a b, Subset b a) => Unifying a b
