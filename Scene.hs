module Scene
( Scene(..)
, Light(..)
) where

import Geometry hiding (direction, position)

data Light = Light { position :: Point
                   , direction :: Vector
                   } deriving (Show)

-- Add lights.
data Scene = Scene { shapes :: [Shape]
                   , lights :: [Light]
                   } deriving (Show)