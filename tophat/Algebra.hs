module Algebra where

import Prelude hiding ((*), (/), (-), (+), recip, negate)

infixl 6 +, -

class AdditiveGroup f where
  zero :: f
  (+) :: f -> f -> f
  (-) :: f -> f -> f
  negate :: f -> f


  a - b = a + negate b
  negate a = zero - a

infixl 7 *, /

class (AdditiveGroup f) => Field f where
  one :: f
  (*) :: f -> f -> f
  (/) :: f -> f -> f
  recip :: f -> f

  a / b = a * recip b
  recip a = one / a

  fromInteger :: Integer -> f
