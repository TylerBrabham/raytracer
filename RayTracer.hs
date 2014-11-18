module RayTracer
( RayTracer(..)
, Camera(..)
, traceScene
, trace
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
                     } deriving (Show)

-- Add deriving show for this type
data RayTracer = RayTracer { scene :: Scene
                           , rayGenerator :: RayGenerator
                           }

-- StandardScreen is a screen at [-1, 1] X [-1, 1] in the xy place.
data RayGenerator = StandardScreen { camera :: Camera } 
                  | SphereLens { camera :: Camera}

-- A function which gives a ray when requested.
generateRay :: RayGenerator -> (Int, Int) -> Ray
generateRay = undefined

-- Get a list of possible hits and map them to actual values using mapMaybe
trace :: Ray -> Scene -> [LocalGeometry]
trace ray sc = mapMaybe (intersect ray) (shapes sc)

-- Primary method to trace every vector
traceScene :: RayTracer -> Bool
traceScene = undefined

