import Geometry
import Scene
import RayTracer
import Numeric.Matrix hiding (trace, map)
import System.Environment

breakUpList :: [a] -> Int -> [[a]] -> [[a]]
breakUpList [] _ acc = acc
breakUpList xs s acc = breakUpList ys s (acc ++ [y])
  where (y, ys) = splitAt s xs

formatPPM :: [Char] -> [Char]
formatPPM xs = concatMap (:"  0  0    ") xs

writePPM :: [Int] -> IO ()
writePPM vals = do
  writeFile "ppm/firstTest.ppm" "P3\n"
  appendFile "ppm/firstTest.ppm" "500 500\n1"

  let strings = map (show) vals
      appendLinePPM = appendFile "ppm/firstTest.ppm"
      instructions = map (\ x -> appendLinePPM $ ('\n': (formatPPM $ concat x))) (breakUpList strings 500 [])
  sequence_ instructions

  -- print . concat . head $ breakUpList strings 10 []
   -- ((formatPPM $ (concat . head) (breakUpList strings 10 [])) ++ ['\n'])

main :: IO ()
main = do
  let rayOrigin = fromList [[0], [0], [2]]
      rayDirection = fromList [[0], [0], [-1]]
      sphereCenter = fromList [[0], [0], [0]] 
      sphereRadius = 1.0

      ray = Ray rayOrigin rayDirection
      sphere = Sphere sphereCenter sphereRadius
      sc = Scene [sphere] []
      cam = Camera (fromList [[0], [0], [5]]) (fromList []) (500, 500)
      generator = StandardScreen cam

      rayTracer = RayTracer sc generator

  writePPM (traceScene rayTracer)