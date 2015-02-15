{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module PersistentHomology where

import Prelude hiding ((-), (/), recip)
import Language.Haskell.TH
import Data.List
import Data.Function
import Data.Maybe
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Lens

import Algebra
import Simplex (Simplex)
import qualified Simplex
import Chain (Chain, (.*))
import qualified Chain
import Barcode

import Debug.Trace

data Slot f a = Slot { _slotSimplex :: Simplex a,
                       _chain :: Maybe (Chain f (Simplex a)),
                       _marked :: Bool } deriving (Show)

makeLenses ''Slot

type Table f a = IM.IntMap (Slot f a)

data PersistentHomology f a = PH { _table :: Table f a,
                                   _indexMap :: M.Map (Simplex a) Int,
                                   _simplexCount :: Int,
                                   _barcode :: Barcode Int }

makeLenses ''PersistentHomology

type PersistentHomologyS f a = State (PersistentHomology f a)

simplexIndex :: (Ord a) => PersistentHomology f a -> Simplex a -> Int
simplexIndex ph s = (ph^.indexMap) M.! s

indexSlot :: (Ord a) => Int -> Lens' (PersistentHomology f a) (Slot f a)
indexSlot i = singular $ table . ix i

slot :: (Ord a) => Simplex a -> Lens' (PersistentHomology f a) (Slot f a)
slot s f ph = indexSlot (simplexIndex ph s) f ph

eliminatePivotRows :: (Field f, Eq f, Ord a) => PersistentHomology f a -> Chain f (Simplex a) -> (Chain f (Simplex a), Int)
eliminatePivotRows ph c | null markedElements = (zero, 0)
                        | isNothing pivotChain = (c, pivotIndex)
                        | isJust pivotChain = eliminatePivotRows ph reduced
    where markedElements = filter (\s -> ph ^. slot s . marked) (Chain.elements c)
          pivotIndex = maximum $ map (simplexIndex ph) markedElements
          pivotSlot = ph^.indexSlot pivotIndex
          pivotSimplex = pivotSlot^.slotSimplex
          pivotChain = pivotSlot^.chain
          pivotCoefficient = Chain.coefficientFor (fromJust pivotChain) pivotSimplex
          chainCoefficient = Chain.coefficientFor c pivotSimplex
          reduced = c - (chainCoefficient / pivotCoefficient) .* fromJust pivotChain

markSimplex :: (Ord a) => Simplex a -> PersistentHomologyS f a ()
markSimplex s = slot s . marked .= True

storeSimplexBoundary :: (Ord a) => Int -> Chain f (Simplex a) -> PersistentHomologyS f a ()
storeSimplexBoundary i c = indexSlot i . chain .= Just c

blankSlot :: Simplex a -> Slot f a
blankSlot s = Slot s Nothing False

startPH :: PersistentHomology f a
startPH = PH { _table = IM.empty,
               _indexMap = M.empty,
               _simplexCount = 0,
               _barcode = [] }

addSimplex :: (Field f, Eq f, Ord a) => Simplex a -> PersistentHomologyS f a ()
addSimplex s = do
    ph <- get
    let index = ph^.simplexCount
    table.at index ?= blankSlot s
    indexMap.at s ?= index
    simplexCount += 1

    ph <- get
    let dimension = Simplex.dimension s
        boundary = Chain.simplexBoundary s
        (reduced, pivotIndex) = eliminatePivotRows ph boundary

    if Chain.empty reduced then
        markSimplex s
    else do
        storeSimplexBoundary pivotIndex reduced
        barcode %= cons (finiteInterval pivotIndex index)

runPH :: (Field f, Eq f, Ord a) => [Simplex a] -> PersistentHomology f a
runPH xs = execState (mapM_ addSimplex xs) startPH

findInfiniteIntervals :: PersistentHomology f a -> Barcode Int
findInfiniteIntervals ph = map (\(index, sl) -> infiniteInterval index) unfilled
    where assocs = IM.assocs (ph^.table)
          unfilled = filter (\(index, sl) -> sl^.marked && isNothing (sl^.chain) ) assocs

finalBarcode :: PersistentHomology f a -> Barcode Int
finalBarcode ph = sort $ ph^.barcode ++ findInfiniteIntervals ph
