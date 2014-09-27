-- file: Maybe'.hs
-- alternative implementation of Maybe

import Prelude             hiding (Maybe)
import Control.Applicative        (Applicative, pure, (<*>))

data Maybe a = Maybe Bool a

instance Show a => Show (Maybe a) where
    show (Maybe True  v) = show v
    show (Maybe False v) = "Nothing"
instance Functor Maybe where
    fmap f (Maybe True  v) = Maybe True  $ f v
    fmap _ (Maybe False _) = Maybe False undefined

instance Applicative Maybe where
    pure v = Maybe True v
    (Maybe True f)  <*> (Maybe True  v) = Maybe True  $ f v
    (Maybe False _) <*> _               = Maybe False undefined
    _               <*> (Maybe False _) = Maybe False undefined

instance Monad Maybe where
    return v = Maybe True v
    (Maybe True  v) >>= f = f v
    (Maybe False _) >>= _ = Maybe False undefined
