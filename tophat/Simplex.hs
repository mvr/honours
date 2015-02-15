module Simplex where

data Simplex a = Simplex { dimension :: Int, vertices :: [a] } deriving (Eq, Ord)

simplex :: [a] -> Simplex a
simplex v = Simplex (length v - 1) v

faces :: Simplex a -> [Simplex a]
faces (Simplex 0 v) = []
faces (Simplex d v) = map (Simplex (d-1)) (deletions v)
    where deletions [] = []
          deletions (x:xs) = xs : map (x:) (deletions xs)

--instance Show (Simplex Char) where
--    show (Simplex d v) = v

instance (Show a) => Show (Simplex a) where
    show (Simplex d v) = concatMap show v