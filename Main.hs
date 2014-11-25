import Geometry
import Scene
import RayTracer
import Numeric.Matrix hiding (trace)
import System.Environment

-- Bottleneck currently is the writing to ppm

breakUpList :: [a] -> Int -> [[a]] -> [[a]]
breakUpList [] _ acc = acc
breakUpList xs s acc = breakUpList ys s (acc ++ [y])
  where (y, ys) = splitAt s xs

-- Cap RGB to 255 so that we don't overflow.
capRGBValues :: (Matrix Int) -> (Matrix Int)
capRGBValues mat = (Numeric.Matrix.map) (min 255) mat

formatPPM :: [Matrix Int] -> [String]
formatPPM vals = Prelude.map (formatRGB) vals

formatRGB :: Matrix Int -> String
formatRGB mat = (show (at cappedMat (1, 1))) ++ "  "
  ++ (show (at cappedMat (2, 1))) ++ "  "
  ++ (show (at cappedMat (3, 1))) ++ "   "
    where cappedMat = capRGBValues mat

writePPM :: [Matrix Int] -> IO ()
writePPM vals = do
  writeFile "ppm/firstTest.ppm" "P3\n"
  appendFile "ppm/firstTest.ppm" "250 250\n255"

  let strings = formatPPM vals
      appendLinePPM = appendFile "ppm/firstTest.ppm"
      instructions = Prelude.map (\ x -> appendLinePPM $ ('\n': (concat x))) (breakUpList strings 250 [])
  sequence_ instructions

main :: IO ()
main = do
  let rayOrigin = fromList [[0], [0], [2]]
      rayDirection = fromList [[0], [0], [-1]]
      sphereCenter = fromList [[0], [0], [0]] 
      sphereCenter2 = fromList [[0], [2], [-1]] 
      sphereRadius = 1.0

      ray = Ray rayOrigin rayDirection
      sphere = Sphere sphereCenter sphereRadius
      sphere2 = Sphere sphereCenter2 sphereRadius
      light = DirectionalLight (fromList [[0.0], [-1.0], [-1.0]])
      sc = Scene [sphere] [light]
      cam = Camera (fromList [[0], [0], [5]]) (fromList []) (250, 250)
      generator = StandardScreen cam

      rayTracer = RayTracer sc generator 2

  writePPM (traceScene rayTracer)