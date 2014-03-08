module WordCount ( wordCount ) where

import Data.Char ( isAlphaNum, isSpace, toLower )
import Data.Map  ( Map, fromListWith )

wordCount :: String -> Map String Int
wordCount  = fromListWith ( + ) . flip  zip ( repeat 1 ) . token

token :: String -> [ String ]
token = words . map ( toLower . replace ) 

replace :: Char -> Char
replace c = if isAlphaNum c || isSpace c then c else ' '


