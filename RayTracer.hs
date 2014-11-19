module RayTracer
( RayTracer(..)
, Camera(..)
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

-- A function which gives a ray when requested. The resolution is doubles but
-- really it should be ints. This currently doesn't take into account the 
-- actual direction of the camera. It just assumes the camera facing towards
-- the origin along the z axis.
generateRay :: RayGenerator -> (Double, Double) -> Ray
generateRay (StandardScreen (Camera p _ (r, s))) (x, y) = 
  Ray p (fromList [[x'],[y'],[0]])
    where
      x' = -1.0 + 2.0 * (x + 0.5) / r
      y' = -1.0 + 2.0 * (y + 0.5) / s

createReflectedRay :: Ray -> LocalGeometry -> Ray
createReflectedRay ray lg = Ray x y
  where 
    x = surfacePoint
    y = undefined

firstHit :: [LocalGeometry] -> LocalGeometry
firstHit = undefined

-- Get a list of possible hits and map them to actual values using mapMaybe
traceRay :: Ray -> Scene -> [LocalGeometry]
traceRay ray sc = mapMaybe (intersect ray) (shapes sc)

-- This method will only work for direction lights.
visibleLight :: Ray -> Light -> Bool
visibleLight = undefined

-- For the ray, call trace, generate a reflected ray, repeat.
hitSequence :: Ray -> Scene -> Int -> [LocalGeometry]
hitSequence _ _ 0 = []
hitSequence ray sc iterationsLeft 
  = geo : hitSequence reflectedRay sc (iterationsLeft - 1)
      where 
        geos = traceRay ray sc
        geo = firstHit geos
        reflectedRay = createReflectedRay ray geo

-- Primary method to trace every vector. Create the list of indices for the 
-- scene. Call map with a function hitSequence, which will essentially map 
-- form pixels to rgb colors.
traceScene :: RayTracer -> Bool
traceScene = undefined

