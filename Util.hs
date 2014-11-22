module Util 
( argMin
) where


-- Non exhaustive
argMin :: (Ord a) => (b -> a) -> [b] -> b
argMin f (b : bs) = argMinHelper f (b : bs) b

argMinHelper f (b : bs) bestSoFar
  | (f b) <= bestSoFar = argMinHelper bs (f b)
  | otherwise = argMinHelper bs (bestSoFar)

-- argMin localTime geos