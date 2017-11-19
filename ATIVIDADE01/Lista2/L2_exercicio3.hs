impar :: Integer -> Bool
impar x = (x `rem` 2) /= 0

etiope :: Integer -> Integer -> Integer
etiope 1 n = n
etiope m n 
  | impar m = n + etiope (m`div`2) (n*2)
  | otherwise = etiope (m`div`2) (n*2)
  
main = do
  print(etiope 7 14)