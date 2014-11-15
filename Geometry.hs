module Geometry
( Shape(..)
, Point (..)
, Vector(..)
, Ray(..)
, LocalGeometry(..)
, intersect
) where

data Point = Point { xCoord :: Float
                   , yCoord :: Float
                   , zCoord :: Float
                   } deriving (Show)

data Vector = Vector { xComp :: Float
                     , yComp :: Float
                     , zComp :: Float
                     } deriving (Show)

data Shape = Circle Point Float deriving (Show)

data Ray = Ray { point :: Point
               , direction :: Vector
               } deriving (Show)

data LocalGeometry = LocalGeometry { surfacePoint :: Point
                                   , normal :: Vector
                                   } deriving (Show)

intersect :: Shape -> Ray -> (Maybe LocalGeometry)
intersect _ _ = Just tempGeo
  where 
    tempGeo = LocalGeometry (Point 0 0 0) (Vector 1 0 0)