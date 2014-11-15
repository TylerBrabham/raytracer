module RayTracer
( RayTracer(..)
, Camera(..)
) where

import Scene
import Geometry

data Camera = Camera { position :: Point
                     , direction :: Vector
                     } deriving (Show)

data RayTracer = RayTracer { scene :: Scene
                           , camera :: Camera
                           } deriving (Show)