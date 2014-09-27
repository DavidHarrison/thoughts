-- file: Simulate.hs
-- Use the Space Comonad instance to simulate concurrent space mutation using
-- prioritized Monoids

simulate :: Monoid m => (a -> SpaceMap m) -> Grid a -> Grid m
