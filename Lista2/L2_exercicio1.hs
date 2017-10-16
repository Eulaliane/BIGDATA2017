ehTriangulo :: (Num a, Ord a) => a -> a -> a -> String
ehTriangulo x y z 
  | (x + y) > z && (x + z) > y && (y + z) > x = " Os lados formam um triangulo"
  | otherwise   = "Os lados nao formam um triangulo"

main = do
  print(ehTriangulo 5 10 9)