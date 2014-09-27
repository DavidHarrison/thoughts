{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}

data Permission a b = Permission a b
data Terminal

data WA
data WB
class HasPerm w p a
instance HasPerm WA p p
instance (HasPerm w p a) => HasPerm WA p (Permission a b)
instance (HasPerm w p b) => HasPerm WB p (Permission a b)

class Subset a b
instance (HasPerm w a b) => Subset a b
instance (Subset a c, Subset b c) => Subset (Permission a b) c

data RIO p a = RIO { runRIO :: IO a }

instance Monad (RIO p) where
    return = RIO . return
    (RIO x) >>= f = RIO $ x >>= runRIO . f

putStrLnR :: (HasPerm WB Terminal r) => String -> RIO r ()
putStrLnR = RIO . putStrLn
