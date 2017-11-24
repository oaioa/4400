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

chain :: (Integral a) => a -> [a] 
chain 1 = [1] 
chain n 
    | even n = n:chain (n `div` 2) 
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

describeListCase :: [a] -> String
describeListCase xs = "the list is "++
                    case xs of
                        []-> "empty"
                        [x]-> "singleton"
                        xs->"longer "


boomBangs:: [Integer]->[[Char]]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   

data Car = Car { brand::String, year::Int }deriving(Show)


data TrafficLight = Redn | Yellown | Greenn deriving(Eq)

data TrafficLightPerso = Red | Yellow | Green

instance Eq TrafficLightPerso where
    Red == Red = True
    Green == Green = True
    Green == Yellow = True
    _ == _ = False

showCar::Car -> String
showCar (Car{brand=a,year=b}) = "A new "++a++" made in "++show b

loopInf:: IO()
loopInf = do
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            loopInf  

reverseWords :: String -> String  
reverseWords = unwords ( map reverse . words) 

someFunc :: IO ()
someFunc = do
    let myCar=Car{brand="peu",year=2000} 
{-    putStrLn ((show $ funcNum 5)
                ++" "++ (show (4 `elem` [0,1,2,3]))
                ++" "++ (head $  boomBangs [20..30])
                ++"\n"++ describeListCase [1,2,3]
                ++"\n"++ (show $ chain 100)++"  "++ show numLongChains
                ++"\n"++showCar myCar
                ++"\n"++ show (Redn==Redn)
                ++"\n"++ show (Red==Red)
                ++"\n"++ show (Green==Red)
                ++"\n"++ show (Green==Yellow)
                ++"\n"++ show (Yellow==Green)
                )
-}  
    loopInf

                
                    
