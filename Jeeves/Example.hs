-- file: Example.hs

import Control.Applicative ((<$>))
import Valet (Valet, mkValet, concrete)

data User = User { name :: String     -- ^ Name of User.
                 , friends :: [User]  -- ^ Friends of User.
                 }

instance Eq User where
  (User na fa) == (User nb fb) = na == nb && fa == fb

jane :: User
joe  :: User
bob  :: User
jane = User  "Jane" []
joe  = User  "Joe"  [jane]
bob  = User  "Bob"  []
relation :: Valet User String
relation = valet (\u -> jane `elem` friends u)
                 "your friend Jane"
                 "your enemy Jane"
relationIO :: Valet User (IO ())
relationIO = putStrLn <$> relation


main :: IO ()
main = (concrete joe relationIO)
          -- "your friend Jane"
       >> (concrete bob relationIO)
          -- "your enemy Jane"
       >> (concrete joe relationIO')
          -- "Say hello to your friend Jane"
       >> (concrete bob relationIO')
          -- "Say hello to your enemy Jane"
  where relationIO' = putStrLn <$> ("Say hello to " ++) <$> relation
