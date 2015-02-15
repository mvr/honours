{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Rational where

import Algebra

instance AdditiveGroup Rational where
  zero = 0
  (+) = (Prelude.+)
  negate = Prelude.negate

instance Field Rational where
  one = 1
  (*) = (Prelude.*)
  recip = Prelude.recip
  fromInteger = fromIntegral
