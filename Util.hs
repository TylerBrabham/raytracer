module Util 
( argMin
) where

-- Non exhaustive
argMin :: (Ord a) => (b -> a) -> [b] -> b
argMin f (b : bs) = argMinHelper f (b : bs) (f b)

argMinHelper :: (Ord a) => (b -> a) -> [b] -> a -> b
-- argMinHelper _ [] bestSoFar = bestSoFar
argMinHelper f (b : bs) bestSoFar
  | (f b) <= bestSoFar = argMinHelper f bs (f b)
  | otherwise = argMinHelper f bs (bestSoFar)

-- argMin localTime geos