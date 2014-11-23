module RayTracer
( RayTracer(..)
, Camera(..)
, RayGenerator(..)
, traceScene
, traceRay
) where

import Data.Maybe
import Geometry
import Numeric.Matrix hiding (map, trace)
import Scene

-- Should add screen to camera, the actual pixels that the rays travel through.
-- A cool thing would be to make something like:
--   Screen :: RayGenerator -> (Int, Int) -> Ray
-- In this way, a screen is not just four vertices defining the corners of a 
-- rectangular screen, but instead a function that generates a ray whenever 
-- one is requested.
data Camera = Camera { position :: Matrix Double
                     , direction :: Matrix Double
                     , resolution :: (Double, Double) -- Assume bl to tr
                     } deriving (Show)

-- Add deriving show for this type
data RayTracer = RayTracer { scene :: Scene
                           , rayGenerator :: RayGenerator
                           }

-- StandardScreen is a screen at [-1, 1] X [-1, 1] in the xy place.
data RayGenerator = StandardScreen Camera

calculateBRDFCoeff :: LocalGeometry -> Light
calculateBRDFCoeff = undefined

-- A function which gives a ray when requested. The resolution is doubles but
-- really it should be ints. This currently doesn't take into account the 
-- actual direction of the camera. It just assumes the camera facing towards
-- the origin along the z axis.
generateRay :: RayGenerator -> (Int, Int) -> Ray
generateRay (StandardScreen (Camera p _ (r, s))) (x, y) = 
  Ray p (fromList [[x'],[y'],[1]] - p)
    where
      x' = -1.0 + 2.0 * ((fromIntegral x) + 0.5) / r
      y' = -1.0 + 2.0 * ((fromIntegral y) + 0.5) / s

-- Get a list of possible hits and map them to actual values using mapMaybe
traceRay :: Ray -> Scene -> Int
traceRay ray sc = if length geos > 0 then 1 else 0
  where
    geos = mapMaybe (intersect ray) (shapes sc)

-- This method will only work for direction lights.
visibleLight :: Ray -> Scene -> Light -> Bool
visibleLight = undefined
-- visibleLight r s l = traceRay r s 

-- Use multi filter, filtering over (Ray, Light) or something like that.
visibleLights = undefined

-- -- For the ray, call trace, generate a reflected ray, repeat.
-- hitSequence :: Ray -> Scene -> Int -> [LocalGeometry]
-- hitSequence _ _ 0 = []
-- hitSequence ray sc iterationsLeft 
--   = geo : hitSequence reflectedRay sc (iterationsLeft - 1)
--       where 
--         geos = traceRay ray sc
--         geo = firstHit geos
--         reflectedRay = createReflectedRay ray geo

lightsSequence :: [LocalGeometry] -> [Light]
lightsSequence = undefined

sumRGBGeo :: (Matrix Double) -> LocalGeometry -> (Matrix Double)
sumRGBGeo = undefined

-- Make an RGB class for this, otherwise it's a bit janky
sumRGB :: [LocalGeometry] -> (Matrix Double)
sumRGB geos = foldl sumRGBGeo (fromList [[0], [0], [0]]) geos

-- Primary method to trace every vector. Create the list of indices for the 
-- scene. Call map with a function hitSequence, which will essentially map 
-- form pixels to rgb colors.
traceScene :: RayTracer -> [Int]
traceScene (RayTracer sc gen) = [traceRay (generateRay gen (x, y)) sc | x <- [1..500], y <- [1..500]]



