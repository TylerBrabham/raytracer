module Geometry
( Shape(..)
, LocalGeometry(..)
, Ray(..)
, intersect
) where

import Numeric.Matrix

data Shape = Sphere { center :: Matrix Double
                    , radius :: Double
                    } deriving (Show)

data Ray = Ray { position :: Matrix Double
               , direction :: Matrix Double
               } deriving (Show)

data LocalGeometry = LocalGeometry { surfacePoint :: Matrix Double
                                   , normal :: Matrix Double
                                   } deriving (Show)

-- Makes no assumption about length.
dotProd x y = at (times (transpose x) (y)) (1,1)

{-
  Return LocalGeometry object if we actually hit something from the outside.
-}
intersect :: Shape -> Ray -> (Maybe Double)
intersect shape ray
  | middleTerm > 0 && t0 > 0 = Just t0
  | otherwise = Nothing
    where 
      a = dotProd (direction ray) (direction ray)
      b = dotProd (scale (direction ray) 2.0) (position ray - center shape)
      c = (dotProd (position ray - center shape) 
          (position ray - center shape)) - (radius shape) ^ 2
      middleTerm = b ^ 2 - 4.0 * a * c
      otherTerm = sqrt middleTerm
      t0 = (-b - otherTerm) / (2 * a)
