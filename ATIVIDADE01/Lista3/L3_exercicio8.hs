collatzLen :: Int -> Int
collatzLen x
  | x < 1 = error "Numero menor do que 1"
  | x == 1 = 1
  | x `mod` 2 == 0 = 1 + collatzLen (x `div` 2)
  | otherwise = 1 + collatzLen (3*x + 1)

collatzEuler14 :: Int -> Int
collatzEuler14 n = snd $ foldl process (0, 0) [n, n-1..1]
  where
    process m n' | processed = m
                 | otherwise = max m (collatzLen n', n')
      where
        processed = n' * 2 <= n


main = do
  print(collatzEuler14 999999)