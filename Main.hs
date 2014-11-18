import Geometry
import Scene
import RayTracer
import Numeric.Matrix hiding (trace)

main :: IO ()
main = do
  let rayOrigin = fromList [[0], [0], [2]]
      rayDirection = fromList [[0], [0], [-1]]
      sphereCenter = fromList [[0], [0], [0]] 
      sphereRadius = 1.0

      ray = Ray rayOrigin rayDirection
      sphere = Sphere sphereCenter sphereRadius
      sc = Scene [sphere] []

  print $ trace ray sc