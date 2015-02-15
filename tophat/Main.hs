module Main where

import PersistentHomology
import Rational
import Z2
import Simplex
import Chain

filtration :: [Simplex Char]
filtration = map simplex ["a", "b", "ab", "c", "ac", "bc"]

main :: IO ()
main = print (finalBarcode (runPH filtration :: PersistentHomology Z2 Char))