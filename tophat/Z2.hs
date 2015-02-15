module Z2 where

import Prelude hiding ((*), (/), (-), (+), recip, negate)
import Algebra

data Z2 = Zero | One deriving (Show, Eq)

instance AdditiveGroup Z2 where
  zero = Zero
  Zero + Zero = Zero
  Zero + One  = One
  One  + Zero = One
  One  + One  = Zero
  negate = id

instance Field Z2 where
  one = One
  Zero * _ = Zero
  One  * a = a
  recip = id

  fromInteger n | even n    = Zero
                | otherwise = One
