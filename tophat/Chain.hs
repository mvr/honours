{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Chain where

import Prelude hiding ((*), (+), (-), (>), fromInteger, negate)
import Simplex
import Algebra
import qualified Data.Map as M

type Chain f b = M.Map b f

coefficientFor :: (Ord b) => Chain f b -> b -> f
coefficientFor c b = c M.! b

simplexBoundary :: (Field f, Ord a) => Simplex a -> Chain f (Simplex a)
simplexBoundary s = M.fromList $ zip (faces s) (map fromInteger (cycle [1, -1]))

elements :: Chain f b -> [b]
elements = M.keys

clean :: (Field f, Eq f, Ord b) => Chain f b -> Chain f b
clean = M.filter (/= zero)

empty :: (Field f, Eq f) => Chain f b -> Bool
empty c = all (== zero) (M.elems c)

instance (Field f, Eq f, Ord b) => AdditiveGroup (Chain f b) where
    zero = M.empty
    c + c' = clean $ M.unionWith (+) c c'
    negate = M.map negate

infixl 7 .*
(.*) :: (Field f, Ord b) => f -> Chain f b -> Chain f b
x .* c = M.map (x*) c
