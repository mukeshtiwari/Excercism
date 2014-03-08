module DNA  (hammingDistance) where

hammingDistance :: String -> String -> Int 
hammingDistance xs = length . filter ( == False ) . zipWith ( == ) xs
