module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise =
    Just . snd . until cond repFun $ (n, 0) where
      
      cond :: (Integer, a) -> Bool
      cond (x, _) = x == 1

      repFun :: (Integer, Integer) -> (Integer, Integer)
      repFun  (x, y)
        | even x = (div x 2, y + 1)
        | otherwise = (3 * x + 1, y + 1)
