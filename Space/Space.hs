-- file: ZipperLife.hs
-- Conway's game of life using zippers
-- largely taken from https://speakerdeck.com/dmoverton/comonads-in-haskell
-- which implementes a 2D zipper comonad, this is an attempt to generalize it
-- to n-dimensional spaces

{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances #-}

import Control.Applicative ( Applicative, (<*>), (<$>), pure )
import Control.Comonad     ( Comonad, extract, duplicate, extend )
import Control.Monad       ( (>=>), join )
import Data.List           ( intersperse, intercalate )
import Data.Foldable       ( Foldable, foldMap )
import Data.Maybe          ( isJust, catMaybes )
import Data.Monoid         ( mappend, mempty )
import System.Posix.Unistd ( nanosleep )
import Test.QuickCheck     ( Arbitrary, arbitrary )
import Test.QuickCheck.Gen ( Gen, choose, generate, vectorOf, suchThat )
import System.IO.Unsafe (unsafePerformIO)

data Space a where
    Point :: a -> Space a
    Space :: [Space a] -> Space a -> [Space a] -> Space a
    deriving (Eq)

instance Arbitrary a => Arbitrary (Space a) where
    arbitrary = (,) <$> return maxDims -- choose (0,maxDims)
                    <*> return maxLength -- choose (0,maxLength)
                >>= uncurry arbS
      where
          maxDims   = 2
          maxLength = 10
          arbS :: Int -> Int -> Gen (Space a)
          arbS 0 _      = Point <$> arbitrary
          arbS dims len = Space <$> arbList (dims - 1) len
                                <*> arbS    (dims - 1) len
                                <*> arbList (dims - 1) len
          arbList :: Int -> Int -> Gen [(Space a)]
          arbList dims len = vectorOf len (arbS dims len)

dims :: Space a -> Int
dims = dims' 0
  where dims' z (Point _) = z
        dims' z (Space _ h _) = dims' (z + 1) h

instance Show (Space a) where
    show = show' True
      where
        show' True  (Point x) = "[" ++ show x ++ "]"
        show' False (Point h) = "\"" ++ show h ++ "\""
        show' c s@(Space bs h fs)
            | dims s == 1 = intercalate " " $ map (show' False) (reverse bs)
                            ++ [show' c h] ++ map (show' False) fs
            | dims s == 2 = intercalate "\n" $ map (show' False) (reverse bs)
                            ++ [show' c h] ++ map (show' False) fs
            | otherwise = (show $ dims s) ++ "-space"

instance Functor Space where
    fmap f (Point      h   ) = Point $ f h
    fmap f (Space bs h fs) = Space (map f' bs) (f' h) (map f' fs)
      where f' = fmap f

instance Comonad Space where
    extract (Point   h) = h
    extract (Space _ h _) = extract h
    extend f s = extend' s 0 f s

extend' :: forall a. forall b. -- not quite sure, just need these here to force
                               -- Haskell to associate all of the as and bs
           Space a             -- the context space to be used in the extend
                               -- function
           -> Int              -- the dimension to shift operate on in the
                               -- context space
           -> (Space a -> b)   -- the extend function
           -> Space a          -- the space to be extended
           -> Space b          -- the resultant space
extend' s _ f (Point _) = Point $ f s
extend' s n f (Space ls h rs) = Space ls' h' rs'
  where
      ls' :: [Space b]
      ls' = updateSpaces back ls
      h' :: Space b
      h' = extendNext s h
      rs' :: [Space b]
      rs' = updateSpaces forth rs
      updateSpaces :: (Space a -> Maybe (Space a)) -> [Space a] -> [Space b]
      updateSpaces d = zipWith extendNext (focuses d)
      extendNext :: Space a -> Space a -> Space b
      extendNext s' x = extend' s' (n + 1) f x
      focuses :: (Space a -> Maybe (Space a)) -> [Space a]
      focuses d = catMaybes $ drop 1 $ iterate (d `onDim` n) s

instance Foldable Space where
    foldMap = foldMap' mempty
      where
          foldMap' z f (Point x) = z `mappend` f x
          foldMap' z f (Space bs h fs) = foldl (\a v -> foldMap' a f v) z
                                       $ (reverse bs) ++ (h:fs)

back :: Space a -> Maybe (Space a)
back (Point h)           = Nothing
back (Space (b:bs) h fs) = Just $ Space bs b (h:fs)
back s                   = Nothing

forth :: Space a -> Maybe (Space a)
forth (Point h)           = Nothing
forth (Space bs h (f:fs)) = Just $ Space (h:bs) f fs
forth s                   = Nothing

-- TODO, rewrite to use onEachDim
prevnext :: (Space a -> Maybe (Space a)) -- the function to try
         -> (Space a -> Space a)         -- the reset function
         -> Space a                      -- the space
         -> Maybe (Space a)
prevnext f r s = case catMaybes vs of
                      []  -> Nothing
                      vs' -> Just $ (r `onSuccDims` n) $ head vs'
    where
        n = length $ dropWhile (== Nothing) vs
        vs = reverse $ f `onEachDim` s

prev, next :: Space a -> Maybe (Space a)
prev = prevnext back far
next = prevnext forth zero

zero :: Space a -> Space a
zero p@(Point _) = p
zero (Space bs h fs) = Space [] (last bs) (reverse (init bs) ++ [h] ++ fs)

far :: Space a -> Space a
far p@(Point _) = p
far (Space bs h fs) = Space (reverse $ reverse bs ++ h : init fs) (last fs) []

onDim :: Applicative f => (Space a -> f (Space b)) -> Int -> Space a -> f (Space b)
onDim f 0 s = f s
onDim f n (Point _) = error $ show n ++ " too few dimensions"
onDim f n (Space bs h fs) = sequence $ Space (map f' bs) (f' h) (map f' fs)
  where f' = f `onDim` (n - 1)

onAllDims, onPrevDims, onSuccDims :: (Space a -> Space a) -> Space a -> Space a
onAllDims f s = onDims [0..dims s] f s
onSuccDims n f s = onDims [n..dims s]
onPrevDims n f s = onDims [0..n]

onDims :: [Int] -> (Space a -> Space a) -> Space a -> Space a
onDims ns f s = foldl (flip ($)) s $ map (f `onDim`) ns

onEachDim :: (Space a -> Space a) -> Space a -> [Space a]
onEachDim = zipWith ($) (zipWith (f `onDim`) [0..dims s]) $ repeat s

size :: Space a -> Int
size (Point _) = 1
size (Space bs h fs) = length bs + 1 + length fs

nsize :: Space a -> Int
nsize = product . onEachDim size

dist :: (Space a -> Maybe (Space a)) -> Space a -> Int
dist f = length . takeWhile isJust . iterate f

valuesEq :: Eq a => Space a -> Space a -> Bool
valuesEq a b = zero `onAllDims` a == zero `onAllDims` b
-- focusEq :: Space a -> Space a -> Bool

toList :: Space a -> [a]
toList = reverse . toList' []
  where toList' z (Point x) = x:z
        toList' z (Space bs h fs) = foldl (\a -> toList' a) z
                                  $ (reverse bs) ++ (h:fs)
