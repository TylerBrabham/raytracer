Raytracer in Haskell.

Ideas:
  - When tracing a whole scene, for each pixel, get a list of LocalGeometry that
    actually were hit, and call foldl to calculate the value.
  - Use a function to generate rays on the fly instead of just using a screen.
    This will let me have spherical lenses and other stuff.