module MathUtil 
( dotProd
, norm
, normalize
, roundMatrix
) where

import Numeric.Matrix

-- Makes no assumption about length.
dotProd :: Matrix Double -> Matrix Double -> Double
dotProd x y = at (times (transpose x) (y)) (1,1)

-- Doesn't check if the normal is nonzero
normalize :: Matrix Double -> Matrix Double
normalize x = scale x (1.0 / (norm x)) 

norm :: Matrix Double -> Double
norm x = sqrt (dotProd x x)

roundMatrix :: (Matrix Double) -> (Matrix Int)
roundMatrix mat = (Numeric.Matrix.map) round mat