{-# LANGUAGE ExistentialQuantification #-}

import Prelude               hiding (zipWith)
import Control.Monad         (join)
import Control.Applicative   (Applicative, pure, (<$>), (<*>))
import Control.Comonad       (Comonad, extract, duplicate, extend)
import Data.Foldable         (foldMap)
import Data.Functor.Identity (Identity(Identity))
import Data.Key              (Zip, zipWith)
import Data.Monoid           (mempty)
import Data.Traversable      (Traversable, sequenceA)
import Foreign               (Ptr, Storable, castPtr, peek, poke)
import Foreign               (malloc, realloc, free)


data Deferred b = forall a. Storable a => Def (a -> b) (IO (Ptr a))
data Mem a      = forall a. Storable a => Mem (IO (Ptr a))

mkDef :: Storable a => a -> Deferred a
mkDef a = Def id (malloc >>= \p -> poke p a >> return p)

runDef :: Storable a => Deferred a -> IO a
runDef (Def f p) = do
    p' <- p
    a <- peek p'
    free p'
    return $ f a

instance Functor Deferred where
    fmap g (Def f p) = Def (g . f) p

instance Functor Mem where
    fmap = memMap

memMap :: (Storable a, Storable b) => (a -> b) -> Mem a -> Mem b
memMap f (Mem p) = Mem $ p >>= mapPtr (f :: a -> b)

mapPtr :: (Storable a, Storable b) => (a -> b) -> Ptr a -> IO (Ptr b)
mapPtr f p = do
    x <- peek p
    p' <- realloc p
    poke p' $ f x
    return p'

{-
clean :: ReuseIO f (Live a) -> ReuseIO f (Live a)
clean (Reuse s fs) = foldMap id <$> sequenceA (fmap salvage s)
                     >>= (\fs' -> return $ Reuse s $ fs' ++ fs)
  where
      salvage :: Ptr a -> IO [Ptr a]
      salvage p = do
          v <- peek p
          case v of
               Live a -> return mempty
               Dead a -> return (return v)
-}

-- data Live a = Live a | Dead a
{-
data Reuse f a = forall a. Storable a => Reuse (IO (f (Ptr a), [Ptr a]))

instance (Functor f, Traversable f) => Functor (Reuse f) where
    fmap f (Reuse r) = Reuse $ join $ fmap (\(a,b) -> fmap (\a' -> (a',b))
                     $ sequenceA $ fmap (mapPtr f) a) r
-}

{-
mapReuse :: (Functor f, Traversable f, Storable a, Storable b)
        => (a -> b) -> ReuseIO f a -> ReuseIO f b
mapReuse f (Reuse s fs) = (\s' -> Reuse s' fs)
                          <$> (sequenceA $ fmap (mapPtr f) s)
-}

