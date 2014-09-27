type Memo a b = [(a,b)]
data MemoFun  = MemoFun { fun :: (a -> b), memo :: Memo a b }

instance Functor MemoFun where
  fmap f mf = 

instance Applicative MemoFun where
  pure f = MemoFun f []
  (MemoFun fa ma) <*> (MemoFun fb mb)
    | 
