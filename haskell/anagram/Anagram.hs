module Anagram ( anagramsFor ) where
import Data.List
import Data.Char
import Control.Arrow

sortLower ::  String -> String 
sortLower = sort . map toLower

checkAnagram :: String -> String -> Bool
checkAnagram  st x = sortLower st == sortLower x
 
anagramsFor :: String -> [ String ] -> [ String ]
anagramsFor st xs = filter ( \x -> ( /= ) st x  && checkAnagram st x ) xs
