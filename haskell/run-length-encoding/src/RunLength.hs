module RunLength (decode, encode) where
import Data.List
import Data.Char

{-- I love Haskell --}
decode :: String -> String
decode encodedText = decodeInside encodedText where
  decodeInside text =
    case span isNumber text of
      ("", "") -> ""
      ("", (t:ts)) -> t : decodeInside ts
      (num, (t:ts)) -> replicate (read num :: Int) t ++ decodeInside ts
  

encode :: String -> String
encode text =
  concatMap compEncode . group $ text where
  compEncode :: String -> String
  compEncode [x] = [x]
  compEncode str@(t:ts) = show (length str) ++ [t]
