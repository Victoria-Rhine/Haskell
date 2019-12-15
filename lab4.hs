module Main where
import System.Environment

{- Lab 4: Victoria Rhine -}

-- Problem #1
-- Convert gallons to liters
gallons :: (RealFloat a) => a -> a
gallons x = x * 0.264172

-- Convert CAD to USD
usd :: (RealFloat a) => a -> a
usd x = x * 0.75

-- Calculate price
price :: (RealFloat a) => a -> a -> a
price x y = (usd x) / (gallons y)

{- Answer: 62.3 liters of fuel for 78.4 Canadian 
    dollars is $3.58/gallon in US dollars -}

-- Problem #2
flightDistance :: (RealFloat a) => (a, a) -> (a, a) -> a
flightDistance (x1, y1) (x2, y2) = 3963.0 * acos((cos dx1 * cos dx2 * cos(dy1 - dy2)) + (sin dx1 * sin dx2))
    where (dx1, dy1) = degreeToRadian (x1, y1)
          (dx2, dy2) = degreeToRadian (x2, y2)

-- Convert degree to radian
degreeToRadian :: (RealFloat a) => (a, a) -> (a, a)
degreeToRadian (x, y) = ((x*pi)/180, (y*pi)/180)

{- Answer: 2631.8416987553624 is the flight distance between 
the point at 45◦N, 122◦W to the point 21◦N, 158◦W -}

-- Problem #3
sumOddCubes :: (Integral a) => a -> a -> a
sumOddCubes x y = sum[xs | xs <- map (^3) [x..y], odd xs]

{- ANSWER: 1874999250000 is the sum of the cubes 
of all the odd numbers between 1000 and 2000-}

--- Problem #4
-- remove all spaces from a string 
removeSpace x = filter (/= ' ') x
-- passing in "hello there world" evaluates to "hellothereworld"

-- filter all evens from list
filterEvens xs = filter even xs
-- passing in [1,2,3,4,5,6,7,8,9,10] evaluates to [2,4,6,8,10]

-- double every values
doubleValues xs = map (*2) xs
-- passing in [5,10,15] evaluates to [10,20,30]

-- true or false contains num 55
contains55 xs = any (== 55) xs
-- passing in [10,30,55,100] evaluates to true

-- true or false are all in list odd
allOdd xs = all odd xs
-- passing in [2,3,4,5,6] evaluates to false

-- Problem #5
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n  | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
           | otherwise = True
-- passing in 27 evaluates to false
-- passing in 17 evaluates to true
  
-- get primes in a range  
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primes :: [Integer]
primes = eratos [2..]
  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

primesFromTo from to = drop (from-1) $ take to primes
{- ANSWER:  1000th through 1020th prime numbers are 
[7919,7927,7933,7937,7949,7951,7963,7993,8009, 8011,
8017,8039,8053,8059,8069,8081,8087,8089,8093,8101,8111] -}

-- Problem #6
factor :: Integer -> [Integer] 
factor n = removeDuplicates (primeFactors n)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs
              
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
  
{- ANSWER: prime factors of 175561 are [419] -}
{- ANSWER: prie factors of 62451532000 are [2,5,11,13,23,47,101] -}

main=do
                putStrLn "Problem 1:"
                print ( price 78.4 62.3 )
                putStrLn "Problem 2:"
                print ( flightDistance (45,122) (21,158) )
                putStrLn "Problem 3:"
                print ( sumOddCubes 1000 2000 )
                putStrLn "Problem 4:"
                print ( removeSpace "hello there world" )
                print ( filterEvens [1,2,3,4,5,6,7,8,9,10] )
                print ( doubleValues [5,10,15] )
                print ( contains55 [10,30,55,100] )
                print ( allOdd [2,3,4,5,6] )
                putStrLn "Problem 5:"
                print ( isPrime 27 )
                print ( isPrime 17 )
                print ( primesFromTo 1000 1020 )
                putStrLn "Problem 6:"
                print ( factor 175561 )
                print ( factor 62451532000 )
