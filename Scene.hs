module Scene
( Scene(..)
, Light(..)
) where

import Geometry
import Numeric.Matrix

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