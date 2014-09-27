import Data.Vector as V

data Point c = Point 
    { coords :: c
    , neighbours :: [Point c] 
    } deriving (Eq)

type Geometry = Point

data Polygon c = Polygon { bounds :: [Point c] }

data Grid a b = forall a. Ix a => Grid { slices :: [Grid b] } deriving (Eq)

flattenWith :: ([Polygon a] -> b) -> Grid a -> b

mergeWith :: (a -> b -> c) -> Grid a -> Grid c -> Maybe (Grid b)

onDim :: Grid c -> (Grid a -> b) -> Grid a
