-- Find k smallest elements in expected linear time.
-- kSmallest :: [Int] -> Int -> [Int]
-- kSmallest _ 0 = []
-- kSmallest [] _ = []
kSmallest (x : xs) k = do
  lefts <- [y | y <- xs, y <= x]
  return lefts