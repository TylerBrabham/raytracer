import Geometry
import Scene
import RayTracer
import Numeric.Matrix

main = do
  let rayOrigin = fromList [[0], [0], [2]]
      rayDirection = fromList [[0], [0], [-1]]
      sphereCenter = fromList [[0], [0], [0]] 
      sphereRadius = 1.0

      ray = Ray rayOrigin rayDirection
      sphere = Sphere sphereCenter sphereRadius

  print $ intersect sphere ray