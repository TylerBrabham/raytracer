module Geometry
( Shape(..)
, LocalGeometry(..)
, Ray(..)
, intersect
, createReflectedRay
, firstHit
) where

import Numeric.Matrix
import MathUtil
import Util

data Shape = Sphere { center :: Matrix Double
                    , radius :: Double 
                    } deriving (Show)

data Ray = Ray { position :: Matrix Double
               , direction :: Matrix Double
               } deriving (Show)

data LocalGeometry = LocalGeometry { surfacePoint :: Matrix Double
                                   , normal :: Matrix Double
                                   , localTime :: Double
                                   } deriving (Show)

sphereLocalGeometry :: Double -> Ray -> Shape -> LocalGeometry
sphereLocalGeometry t ray sphere = LocalGeometry x y t
  where x = (scale (direction ray) t) + (position ray)
        vec = x - (center sphere)
        y = normalize vec

-- Return LocalGeometry object if we actually hit something from the outside. 
-- Use Maybe so that the caller to this function is required to check that 
-- something was actually hit.
intersect :: Ray -> Shape -> (Maybe LocalGeometry)
intersect ray shape
  | middleTerm > 0 && t0 > 0.001 = Just (sphereLocalGeometry t0 ray shape)
  | otherwise = Nothing
    where 
      a = (norm (direction ray)) ^ 2
      b = dotProd (scale (direction ray) 2.0) (position ray - center shape)
      c = (norm (position ray - center shape)) ^ 2 - (radius shape) ^ 2
      middleTerm = b ^ 2 - 4.0 * a * c
      otherTerm = sqrt middleTerm
      t0 = (-b - otherTerm) / (2 * a)

createReflectedRay :: Ray -> LocalGeometry -> Ray
createReflectedRay ray geo = Ray x y
  where 
    x = surfacePoint geo
    y = l + (scale n s)
    s = 2 * (dotProd l n)
    n = normal geo
    l = direction ray

firstHit :: [LocalGeometry] -> LocalGeometry
firstHit geos = argMin localTime geos