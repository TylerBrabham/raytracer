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
import MathUtil

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
                           , maxDepth :: Int
                           }

-- StandardScreen is a screen at [-1, 1] X [-1, 1] in the xy place.
data RayGenerator = StandardScreen Camera

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

traceRay :: Ray -> Scene -> Bool
traceRay ray sc t = any (<t) timesOfIntersection
  where 
    geos = mapMaybe (intersect ray) (shapes sc)
    timesOfIntersection = map (localTime) geos

-- This method will only work for direction lights.
visibleLight :: Ray -> Scene -> Light -> Bool
visibleLight = undefined
-- visibleLight r s l = traceRay r s 

-- Use multi filter, filtering over (Ray, Light) or something like that.
visibleLights = undefined

-- -- -- For the ray, call trace, generate a reflected ray, repeat.
-- hitSequence :: Ray -> Scene -> Int -> [LocalGeometry]
-- hitSequence _ _ 0 = []
-- hitSequence ray sc iterationsLeft 
--   = geo : hitSequence reflectedRay sc (iterationsLeft - 1)
--       where 
--         geos = traceRay ray sc
--         geo = firstHit geos
--         reflectedRay = createReflectedRay ray geo

calculateBRDF :: LocalGeometry -> Light -> Matrix Int
calculateBRDF geo (DirectionalLight dir) = roundMatrix $ scale (fromList [[255.0], [0.0], [255.0]]) scalar
  where
    n = normal geo
    l = scale dir (-1.0)
    scalar = max (dotProd n l) 0.0

-- Get a list of possible hits and map them to actual values using mapMaybe
traceRayRGB :: Ray -> Scene -> Int -> (Matrix Int)
traceRayRGB _ _ 0 = fromList [[0], [0], [0]]
traceRayRGB ray sc depth
  | length geos > 0 = calculateBRDF (head geos) (head (lights sc)) + (traceRay reflectedRay sc (depth -1))
  | otherwise = fromList[[0], [0], [0]]
    where
      geos = mapMaybe (intersect ray) (shapes sc)
      reflectedRay = createReflectedRay ray (head geos) 

lightsSequence :: [LocalGeometry] -> [Light]
lightsSequence = undefined

sumRGBGeo :: (Matrix Double) -> LocalGeometry -> (Matrix Double)
sumRGBGeo = undefined

-- Make an RGB class for this, otherwise it's a bit janky
sumRGB :: [LocalGeometry] -> (Matrix Double)
sumRGB geos = foldl sumRGBGeo (fromList [[0], [0], [0]]) geos

calculateRGB :: RayTracer -> (Int, Int) -> (Matrix Int)
calculateRGB (RayTracer sc gen mdepth) (x, y) = 
  traceRay (generateRay gen (x, y)) sc mdepth

-- Primary method to trace every vector. Create the list of indices for the 
-- scene. Call map with a function hitSequence, which will essentially map 
-- form pixels to rgb colors.
traceScene :: RayTracer -> [Matrix Int]
traceScene rayTracer = map (calculateRGB rayTracer) pixels
  where pixels = [(x, y) | x <- [1..250], y <- [1..250]]