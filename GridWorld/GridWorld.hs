-- file: GridWorld.hs
-- Functional (programming) implementation of the GridWorld case study used
-- in AP Computer Science curriculum

import Control.Comonad
import Data.Map.Strict as M

data ZipMap k a = ZipMap k (M.Map k v)
data Ix2 = Ix2 Int Int deriving (Eq,Ord)
type Grid a = ZipMap Ix2 a

instance Functor (ZipMap k a) where
    fmap f (ZipMap i m) = ZipMap i $ fmap f m

instance Comonad (ZipMap k a) where
    extract   (ZipMap i m) = m ! i
    duplicate (ZipMap i m) = ZipMap i $ mapWithKey (\k a -> ZipMap k m) m

makeGrid :: (Ix2 -> a) -> Int -> Int -> Grid a
makeGrid f x y = ZipMap (Ix2 0 0) 
               $ fromList $ (\i -> (i,f i)) . Ix2 <$> [0..x] <*> [0..y]
