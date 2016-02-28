module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear n
  | mod n 4 /= 0 = False
  | mod n 100 /= 0 = True
  | mod n 400 /= 0 = False
  | otherwise = True
