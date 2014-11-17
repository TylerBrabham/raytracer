module Scene
( Scene(..)
, Light(..)
) where

import Geometry
import Numeric.Matrix

data Light = Light { position :: Matrix Double
                   , direction :: Matrix Double
                   } deriving (Show)

-- Add lights.
data Scene = Scene { shapes :: [Shape]
                   , lights :: [Light]
                   } deriving (Show)