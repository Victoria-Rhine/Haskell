import Data.List
import Data.Char
import System.Environment 
import System.IO
import Assn2b

-- RECURSION
gcdMine :: Integral a => a -> a -> a
gcdMine x 0 = x
gcdMine x y = gcdMine y (x `mod` y)
-- passing in 111 259 evaluates to 37

gcdCheck x y = (myAnswer, correctAnswer, comment)
        where
        myAnswer = gcdMine x y
        correctAnswer = gcd x y
        comment = if myAnswer == correctAnswer
                    then "Matches"
                    else "Does Not Match"
-- passing in 111 259 evaluates to 37, 37, "Matches"

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
--passing in [0..20] evaluates to 
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

count :: (Eq a, Num b) => a -> [a] -> b
count find [] = 0
count find (x:xs) 
    | find == x = 1 + (count find xs)
    | otherwise = count find xs
-- passing in 7 [1,7,6,2,7,7,7] evaluates to 3

sanitize :: String -> String
sanitize "" = []
sanitize x
       | (head x) == ' ' = '%' : '2' : '0' : sanitize (tail x)
       | otherwise = (head x) : sanitize (tail x)
-- passing in "http://wou.edu/my homepage/I love spaces.html"
-- evaluates to "http://wou.edu/my%20homepage/I%20love%20spaces.html"

-- HIGH ORDER FUNCTIONS I
timesTen = map (*10)
-- passing in [1,2,3,4,5,6] evaluates to [10,20,30,40,50,60]

increment xs = map (succ) xs
-- passing in "Hello" evaluates to "ifmmp"

div42 x = mod x 42 == 0
cont42 xs = filter (div42) xs
yesOrNo42 xs = if cont42 xs == []
                then "False"
                else "True"
-- passing in [13, 42, 100, 3, 84] evaluates to "True"
-- passing in [13, 40, 100, 3, 94] evaluates to "False"

raiseTen = map (10^) 
-- passing in [5,3,8,2,3,6,3] evaluates to 
-- [100000,1000,100000000,100,1000,1000000,1000]

stripString = takeWhile (' ' <)
-- passing in "Hello     " evaluates to "Hello"

isEven x = mod x 2 == 0
allEvenList = all (isEven)
-- passing in [1,4,7,14,10,11] evaluates to false
-- passing in [2,4,6,8,10,12] evaluates to true

addNot = zipWith (++) 
-- passing in ["not ", "not ", "not "] ["funny","cold","slow"]
-- evaluates to ["not funny", "not cold", not slow"]

nestedreverse = map reverse
-- passing in ["This", "is", "a", "sentence"]
-- evaluates to ["sihT","si","a","ecnetnes"]

-- LAMBDA FUNCTIONS
plus = \x -> (\y -> x + y)
-- passing in plus 17 80 evaluates to 97

times4 = \x -> (x + x + x + x)
-- passing in 5 evaluates to 20

secondElement = \xs -> (xs !!1)
-- passing in [2,3,4,5,6] would gives 3

sqrtRounded = \x -> (round (sqrt x))
-- passing in 129 evaluates to 11 
-- square root of 129 is 11.3578...rounded is 11

myWords = \xs -> (words (xs))
compStringToList = \xs -> (\xy -> myWords xs == xy) 
-- passing in "This is a sentence written in the usual way."
-- ["This","is","a","sentence","written","in","the","usual","way."]
-- evaluates to true

pythagTheorem = map (\(a,b) -> (a,b,(sqrt $ a^2 + b^2)))
-- passing in [(3,4),(5,16),(9.4,2)] evaluates to
-- [(3.0,4.0,5.0),(5.0,16.0,16.76305461424021),(9.4,2.0,9.610411021387172)]

-- foldl(*)6[5,3,8] == (((6 * 5) * 3) * 8) = 720
--              *
--             / \
--            *   8
--           / \
--          *   3      
--         / \
--        6   5

-- foldr (*) 6 [5,3,8] == ( 5 * ( 3 * ( 8 * 6 ))) = 720
--              *
--             / \
--            5   *
--               / \
--              3   *
--                 / \
--                8   6

-- Evaluates the same since it's multiplication
-- foldl folds from the left, foldr folds from the right


main=do
        putStrLn "Problem #1:"
        print ( gcdCheck 111 259 )
        print ( gcdCheck 2945 123042 )
        print ( gcdCheck (2*5*7) (7*23) )
        putStrLn "Problem #2:"
        print ( [fibonacci n | n <- [0..20]] )
        putStrLn "Problem #3:"
        print ( count 7 [1,7,6,2,7,7,9] )
        print ( count 'w' "western oregon wolves" )
        putStrLn "Problem #4:"
        print ( sanitize "http://wou.edu/my homepage/I love spaces.html" )
        putStrLn "Problem #5:"
        print ( timesTen [1,2,3,4,5,6] )
        putStrLn "Problem #6:"
        print ( increment [5,6,7,8,9,10] )
        print ( increment ['H','e','l','l','o'] )
        putStrLn "Problem #7:"
        print ( yesOrNo42 [13,42,100,3,84] )
        print ( yesOrNo42 [13,40,100,3,94] )
        putStrLn "Problem #8:"
        print ( raiseTen [5,3,8,2,3,6,3] )
        putStrLn "Problem #9:"
        print ( stripString "Hello     " )
        putStrLn "Problem #10:"
        print ( allEvenList [1,4,7,14,10,11] )
        print ( allEvenList [2,4,6,8,10,12] )
        putStrLn "Problem #11:"
        print ( addNot ["not ", "not ", "not "] ["funny","cold","slow"] )
        putStrLn "Problem #12:"
        print ( nestedreverse ["This", "is", "a", "sentence"] )
        putStrLn "Problem #13:"
        print ( plus 17 80 )
        putStrLn "Problem #14:"
        print ( times4 5 )
        putStrLn "Problem #15:"
        print ( secondElement [2,3,4,5,6] )
        putStrLn "Problem #16:"
        print ( sqrtRounded 129 )
        putStrLn "Problem #17:"
        print ( compStringToList "This is a sentence written in the usual way." ["This","is","a","sentence","written","in","the","usual","way."] )
        putStrLn "Problem #18:"
        print ( pythagTheorem [(3,4),(5,16),(9.4,2)] )
        putStrLn "----------\nProblem #2:"
        print ( lengthFold [1,2,3,4,5] )
        putStrLn "Problem #3:"
        print ( convertIntToStringLeft [5,2,8,3,4] )
        print ( convertIntToStringRight [5,2,8,3,4] ) 
        putStrLn "Problem #4:"
        print ( length $ filter (<20) [1..100] )
        print ( take 10 $ cycle $ filter (>5) $ map (*2) [1..10] )
   
        