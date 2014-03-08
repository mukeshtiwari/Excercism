module DNA ( count, nucleotideCounts ) where
import qualified Data.Map.Strict as M 

nucleotideCounts :: String -> M.Map Char Int 
nucleotideCounts  st = M.unionWith ( + ) 
                       ( M.fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)] )
                       ( M.fromListWith ( + ) . flip zip ( repeat 1 ) $ st   )

count :: Char -> String -> Int
count x xs = case x `elem` [ 'A', 'C', 'T', 'U' ] of 
               True -> length . filter ( == x ) $ xs 
               _  -> error $ "invalid nucleotide " ++ show x
 
