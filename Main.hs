import Geometry
import Scene
import RayTracer

main = do
  let center = Point 0 0 0
      circle = Circle center 5
      ray = Ray center (Vector 1 0 0)
      camera = Camera (Point 0 0 0) (Vector 1 0 0)
      scene = Scene [circle] []

  print $ scene