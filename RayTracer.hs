module RayTracer
( RayTracer(..)
, Camera(..)
, traceScene
) where

import Scene
import Geometry

data Camera = Camera { position :: Point
                     , direction :: Vector
                     } deriving (Show)

data RayTracer = RayTracer { scene :: Scene
                           , camera :: Camera
                           } deriving (Show)

-- Private helper method to trace a single ray at a time. This should not 
-- actually return a bool. Instead, we should get a list of Maybe LocalGeometry
-- objects and then filterM over them to get a Maybe List. And finally, we 
-- should find the closest shape in the MaybeList, calculate the brdf/rgb values
-- and pass the result along to a function to add the values together.
trace :: RayTracer -> Ray -> Bool
trace = undefined

-- Primary method to trace every vector
traceScene :: RayTracer -> Bool
traceScene = undefined

