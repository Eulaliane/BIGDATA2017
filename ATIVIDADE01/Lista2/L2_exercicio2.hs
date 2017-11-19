ehTriangulo :: (Num a, Ord a) => a -> a -> a -> String
ehTriangulo x y z 
  | (x + y) < z || (x + z) < y || (y + z) < x = "Os lados nao formam um triangulo"
  |(x==y) && (y==z) = "Equilatero"
  |(x/=y) && (y/=z) = "Escaleno"
  |otherwise = "Isosceles"
  
main = do
  print(ehTriangulo 5 10 9)