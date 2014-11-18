module RayTracer
( RayTracer(..)
, Camera(..)
, traceScene
, trace
) where

import Numeric.Matrix hiding (map, trace)

import Scene
import Geometry

-- Should add screen to camera, the actual pixels that the rays travel through
data Camera = Camera { position :: Matrix Double
                     , direction :: Matrix Double
                     } deriving (Show)

data RayTracer = RayTracer { scene :: Scene
                           , camera :: Camera
                           } deriving (Show)

-- This function can be replaced with a built in one. Check Maybe package.
extractMaybe (Just a) = a

-- Private helper method to trace a single ray at a time. This should not 
-- actually return a bool. Instead, we should get a list of Maybe LocalGeometry
-- objects and then filterM over them to get a Maybe List. And finally, we 
-- should find the closest shape in the MaybeList, calculate the brdf/rgb values
-- and pass the result along to a function to add the values together.
trace :: Ray -> Scene -> [LocalGeometry]
trace ray sc = map extractMaybe actualHits
  where 
    possibleHits = map (intersect ray) (shapes sc)
    actualHits = filter (hit) possibleHits


-- Primary method to trace every vector
traceScene :: RayTracer -> Bool
traceScene = undefined

