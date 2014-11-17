module Geometry
( Shape(..)
, LocalGeometry(..)
, Ray(..)
, intersect
, hit
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

hit :: (Maybe LocalGeometry) -> Bool
hit (Just x) = True
hit Nothing = False

sphereLocalGeometry t ray sphere = LocalGeometry x y
  where
    x = (scale (direction ray) t) + (position ray)
    y = x - (center sphere)

{-
  Return LocalGeometry object if we actually hit something from the outside. 
  Use Maybe so that the caller to this function is required to check that 
  something was actually hit.
-}
intersect :: Ray -> Shape -> (Maybe LocalGeometry)
intersect ray shape
  | middleTerm > 0 && t0 > 0 = Just (sphereLocalGeometry t0 ray shape)
  | otherwise = Nothing
    where 
      a = dotProd (direction ray) (direction ray)
      b = dotProd (scale (direction ray) 2.0) (position ray - center shape)
      c = (dotProd (position ray - center shape) 
          (position ray - center shape)) - (radius shape) ^ 2
      middleTerm = b ^ 2 - 4.0 * a * c
      otherTerm = sqrt middleTerm
      t0 = (-b - otherTerm) / (2 * a)
