Raytracer in Haskell.

Ideas:
  - Use trace method to get a list of Maybe LocalGeometry. filterM These to the
    actual LocalGeometry objects, if any are not Nothing.
  - When tracing a whole scene, for each pixel, get a list of LocalGeometry that
    actually were hit, and call foldl to calculate the value.