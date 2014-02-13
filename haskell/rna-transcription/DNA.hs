module DNA ( toRNA ) where 


rnafun :: Char -> Char
rnafun 'C' = 'C'
rnafun 'G' = 'G'
rnafun 'A' = 'A'
rnafun 'T' = 'U'

toRNA :: String -> String
toRNA  = map rnafun  
