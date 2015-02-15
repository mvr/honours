module Barcode where

data WithInfinity a = Finite a | Infinity deriving (Eq, Ord)

data Interval a = Interval { start :: a, end :: WithInfinity a } deriving (Eq, Ord)

type Barcode a = [Interval a]

instance (Show a) => Show (WithInfinity a) where
    show (Finite a) = show a
    show Infinity = "Inf"

instance (Show a) => Show (Interval a) where
    show (Interval start end) = show start ++ " -> " ++ show end

finiteInterval :: a -> a -> Interval a
finiteInterval start end = Interval start (Finite end)

infiniteInterval :: a -> Interval a
infiniteInterval start = Interval start Infinity