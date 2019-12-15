-- Problem fixed is in problem #16
-- had + where there should have been a *

module Main where
import Data.List
import System.Environment
import System.IO

-- Problem #1
-- Get square root of number
intSquareRoot n = sqrt n

-- Problem #2 
-- ASCII character before uppercase char
beforeChar x = pred x

-- Problem #3
-- True or False does a num plus one equal even num
plusOneIsEven x = mod (x + 1) 2 == 0

-- Problem #4
-- Product of all numbers 1 - 100
prodOneToHundred xs = product xs

-- Problem #5
-- Largest number in middle of list
getMaxInMiddle xs =
        let xs1 = tail xs     
        in maximum xs1
                    
-- Problem #6
-- Construct list with construction operator and empty list
constructedList = 6 : 19 : 41 : -3 :[]

-- Problem #7
-- first 27 even numbers
firstEvens x = take x [0, 2..]

-- Problem #8
-- All odds less than 200 divisible by 3 and 7
oddsDiv3And7 xs = [y + 2 | y <- xs, mod (y + 2) 3 == 0 && mod (y + 2) 7 == 0]

-- Problem #9
-- Number of odd between 100-200 divisible by 9
oddsDiv9 xs = [y + 2 | y <- xs, mod (y + 2) 9 == 0]

-- Problem #10
-- Count how many negative numbers in list
lengthNeg xs = length [y + 1 | y <- xs, y < 0]

-- Problem #11
-- Hex mappings with zip, Texas rangers, and concatenation
hexMapping xs xt xu = zip xs (xt++xu)

-- Problem #12
-- Make a list of lists
listOfLists n = [ [1..m] | m <- [1..n] ]


-- Problem #13
-- Replace empty space in string
escape ' ' = "%20"
escape c = [c]
sanitize  = concatMap escape

-- Problem #14
-- :t (*)
--(*) :: Num a => a -> a -> a
-- anything that's a number such as fractional, float, double, and complex number

-- :t (++)
-- (++) :: [a] -> [a] -> [a]
-- concatenates two lists or strings 

-- :t succ
-- succ :: Enum a => a -> a
-- enumerated types that can be ordered such as numeric types 
-- like int, integer, float, double and char

-- :t div
-- div :: Integral a => a -> a -> a
-- divides numbers and can take integral, ratio, int

-- :t head
-- head :: [a] -> a
-- returns first thing from list so it can be used with lists and also strings

-- Problem #15
-- Associate a number with string
getSuit x = 
        if x == 0
            then "Heart"
            else if x == 1
                then "Diamond"
                    else if x == 2
                        then "Spade"
                            else if x == 3
                            then "Club"
                            else "error"
            
-- Problem #16
-- Get dot product of two tuples
dotProduct :: (Double,Double,Double) -> (Double,Double,Double) -> Double
dotProduct (x1, x2, x3) (y1, y2, y3) = ((x1 * y1) + (x2 * y2) + (x3 * y3))

-- Problem #17
reverseFirstThree [] = []
reverseFirstThree [x] = [x]
reverseFirstThree [x, y] = [y, x]
reverseFirstThree (x:y:z:xs) = z: y: x: xs

-- Problem #18
feelsLike x
    | x <= 0 = "Frostbite central!"
    | x > 0 && x <= 32 = "Freezing!"
    | x > 32 && x <= 50 = "Cold, but not freezing"
    | x > 50 && x <= 80 = "It's getting warm"
    | otherwise = "It's hot!"

-- Problem #19
feelsLike2 x
    | x <= 0 = (temp, "Frostbite central!")
    | x > 0 && x <= 32 = (temp, "Freezing!")
    | x > 32 && x <= 50 = (temp, "Cold, but not freezing")
    | x > 50 && x <= 80 = (temp, "It's getting warm")
    | otherwise = (temp, "It's hot!")
    where temp = x * 9/5 + 32

-- Problem #20
cylinderToVolume xs = [v| (r, h) <- xs, let v = pi * r ^ 2 * h]

main=do
        putStrLn "Problem 1:"
        print (intSquareRoot 818281336460929553769504384519009121840452831049)
        putStrLn ""
        
        putStrLn "Problem 2:"
        print (beforeChar 'A')
        putStrLn ""
        
        putStrLn "Problem 3:"        
        print (plusOneIsEven 5)
        putStrLn ""
        
        putStrLn "Problem 4:"
        print (prodOneToHundred [1, 3..100])
        putStrLn ""
        
        putStrLn "Problem 5:"
        print (getMaxInMiddle [99,23,4,2,67,82,49,-40])
        putStrLn ""
        
        putStrLn "Problem 6:"
        print (constructedList)
        putStrLn ""
        
        putStrLn "Problem 7:"
        print (firstEvens 27)
        putStrLn ""
        
        putStrLn "Problem 8:"
        print(oddsDiv3And7 [1,3..200])
        putStrLn ""
        
        putStrLn "Problem 9:"
        print(oddsDiv9 [101, 103..200])
        putStrLn ""
        
        putStrLn "Problem 10:"
        print(lengthNeg [-4,6,7,8,-14])
        putStrLn ""
        
        putStrLn "Problem 11:"
        print (hexMapping [0,1..15] ['0', '1'..'9'] ['A','B'..'F'])
        putStrLn ""
        
        putStrLn "Problem 12:"
        print (listOfLists 3)
        putStrLn ""
        
        putStrLn "Problem 13:"
        print (sanitize "http://wou.edu/my homepage/I love spaces.html")
        putStrLn ""
        
        putStrLn "Problem 14: Commented Out"
        putStrLn ""
        
        putStrLn "Problem 15:"
        print (getSuit 0)
        print (getSuit 4)
        putStrLn ""
        
        putStrLn "Problem 16:"
        print (dotProduct (1, 2, 3.0) (4.0, 5, 6))
        putStrLn ""
        
        putStrLn "Problem 17:"
        print (reverseFirstThree [1, 5, 2, 7, 9])
        putStrLn ""
        
        putStrLn "Problem 18:"
        print (feelsLike (-45))
        print (feelsLike 90)
        putStrLn ""
        
        putStrLn "Problem 19:"
        print (feelsLike2 20)
        print (feelsLike2 72)
        putStrLn ""
        
        putStrLn "Problem 20:"
        print (cylinderToVolume [(2, 5.3), (4.2, 9), (1,1), (100.3, 94)])
