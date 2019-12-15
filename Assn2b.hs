module Assn2b
(
lengthFold,
convertIntToStringLeft,
convertIntToStringRight
) where

import Data.Char
import Data.List
import System.Environment 

lengthFold :: [a] -> Int
lengthFold xs = foldr (\_ y -> y + 1) 0 xs
-- passing in [1,2,3,4,5] evaluates to 5

convertIntToStringLeft :: [Int] -> [Char]
convertIntToStringLeft x = foldl (\acc x -> acc ++ [intToDigit x]) [] x
-- passing in [5,2,8,3,4] evaluates to "52834"

convertIntToStringRight :: [Int] -> [Char]
convertIntToStringRight x = foldr (\x xs -> intToDigit x : xs) [] x
-- passing in [5,2,8,3,4] evaluates to "52834"

--length (filter (<20) [1..100])
--Rewritten as:
-- length $ filter (<20) [1..100]

--take 10 (cycle (filter (>5) (map (*2) [1..10])))
-- Rewritten as:
--take 10 $ cycle $ filter (>5) $ map (*2) [1..10]



