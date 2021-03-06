module Scene
( Scene(..)
, Light(..)
, rayToLight
) where

import Geometry
import Numeric.Matrix

-- For directional lights, the values given in the Matrix Double represent the 
-- direction the light is points in. For point light, the Matrix Double is the
-- location of the light in space.
data Light = PointLight (Matrix Double)
           | DirectionalLight (Matrix Double) deriving (Show)

-- Add lights.
data Scene = Scene { shapes :: [Shape]
                   , lights :: [Light]
                   } deriving (Show)



-- Must pass in position as first argument.
rayToLight :: (Matrix Double) -> Light -> Ray
rayToLight pos (DirectionalLight direction) = Ray pos (-1.0 * direction)
rayToLight pos (PointLight lightPos) = Ray pos (lightPos - pos)