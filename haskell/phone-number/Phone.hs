module Phone (areaCode, number, prettyPrint) where
import Data.Char
import Control.Arrow 

isValid :: String -> String
isValid xs 
  | n < 10  = "0000000000"
  | n == 10 = xs
  | n == 11 && first == '1' = ret
  | n >= 11 = "0000000000" where
     ( first , ret ) = ( head &&& tail ) xs
     n = length xs
  
number :: String -> String 
number  = isValid . filter isDigit


areaCode :: String -> String
areaCode = take 3 . number 

prettyPrint :: String -> String
prettyPrint  = pretty . number where 
     pretty :: String -> String
     pretty  xs =   "(" ++ xs' ++ ") " ++ ys' ++ "-" ++ ret' where
        ( xs', ret  )  = splitAt 3 xs
        ( ys', ret' )  = splitAt 3 ret  
