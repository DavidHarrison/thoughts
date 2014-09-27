-- file: Sequence.hs
-- provide list-like instances for wrapped functions f : Int -> b

import Data.Foldable as F
import Data.Monoid   as M

data Sequence a = Sequence { f :: (Int -> a), len :: Maybe Int }

instance F.Foldable Sequence where
    foldMap m (Sequence f (Just l)) = mconcat $ map (m . f) [0..(l - 1)]
    foldMap m (Sequence f Nothing)  = mconcat $ map (m . f) [0..]
