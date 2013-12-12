{-# LANGUAGE GADTs #-}
module Bob ( responseFor ) where
import Data.Char 
{-- 
 if it contains ? in the end the response is Sure 
 if it contains all the capital letters then Woah 
 if it contains empty or string of spaces then "Fine, be that way"
 otherwise Whatever 
--}

data Response where 
	Sure :: Response 
        Woah :: Response 
        Fine :: Response
        What :: Response 


responseOption :: Response -> String 
responseOption Sure = "Sure."
responseOption Woah = "Woah, chill out!"
responseOption Fine = "Fine. Be that way!"
responseOption What = "Whatever."

responseString :: String -> Response
responseString s 
   | all isSpace  s = Fine
   | ( any isAlpha s ) && ( all isUpper . filter isAlpha $ s ) = Woah
   | last s == '?' = Sure
   | otherwise = What 

responseFor :: String -> String 
responseFor = responseOption . responseString     


{-
responseFor :: String -> String
responseFor "Tom-ay-to, tom-aaaah-to." = "Whatever."
responseFor "WATCH OUT!" = "Woah, chill out!"
responseFor "Does this cryogenic chamber make me look fat?" = "Sure."
responseFor "Let's go make out behind the gym!" = "Whatever."
responseFor "It's OK if you don't want to go to the DMV." = "Whatever."
responseFor "WHAT THE HELL WERE YOU THINKING?" = "Woah, chill out!"
responseFor (
    "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!") = "Woah, chill out!"
responseFor "1, 2, 3 GO!" = "Woah, chill out!"
responseFor "I HATE YOU" = "Woah, chill out!"
responseFor "Ending with ? means a question." = "Whatever."
responseFor "" = "Fine. Be that way!"
responseFor "    " = "Fine. Be that way!"
responseFor ":) ?" = "Sure."
responseFor "\nDoes this cryogenic chamber make me look fat? \nno" = "Whatever."
responseFor "\n\r \t\v\xA0\x2002" = "Fine. Be that way!"
responseFor "1, 2, 3" = "Whatever."
responseFor "4?" =  "Sure."
responseFor "\xdcML\xc4\xdcTS!" = "Woah, chill out!"
responseFor "\xdcML\xe4\xdcTS!" = "Whatever."
-}
