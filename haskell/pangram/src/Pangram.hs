module Pangram (isPangram) where
import Data.Char

isPangram :: String -> Bool
isPangram [] = False
isPangram text = 
  and . map (flip elem ltext) $ ['a' .. 'z'] where
  ltext = map toLower text
