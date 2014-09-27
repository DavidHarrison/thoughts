data Series a b = Series { f :: (a -> b) }

instance Functor (Series a) where
  fmap = liftM
instance Applicative (Series a) where
  pure = return
  (<*>) = ap
instance Monad (Series a) where
  return = Series . const
  (Series f) >>= k = Series $ f >>= k

instance Enum a => Traversable (Series a) where
  traverse = 
