module Lib
    ( someFunc
    ) where

defined = 20

funcNum::Integer->Integer
funcNum x =defined +   
    if x > 10
        then 0
        else x
 
describeListWhere :: [a] -> String  
describeListWhere xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

describeListCase :: [a] -> String
describeListCase xs = "the list is "++
                    case xs of
                        []-> "empty"
                        [x]-> "singleton"
                        xs->"longer "


boomBangs:: [Integer]->[[Char]]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   

someFunc :: IO ()
someFunc = do 
    putStrLn ((show $ funcNum 5)
                ++" "++ (show (4 `elem` [0,1,2,3]))
                ++" "++ (head $  boomBangs [20..30])
                ++"\n"++ describeListCase [1,2,3]
                )

                
                    
