-- Informatics 1 - Functional Programming 
-- Class Test 2020

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: [Int] -> Bool
f xs = and [x>10| x<-xs, odd x]

-- b

g :: [Int] -> Bool
g [] = True
g (x:xs) | odd x, x<= 10 = False && g xs
       | otherwise     = True  && g xs  

-- c

h :: [Int] -> Bool
h [] = True
h xs = foldl (&&) True (map even (filter (<=10) xs))

-- d

prop_fgh :: [Int] -> Bool
prop_fgh xs = 
    f xs == g xs && g xs == h xs

-- Problem 2

-- a

c :: String -> Bool
c [] = False 
c xs = or [x==y | (x,y)<-(zip (init xs) (drop 1 xs))]

-- b


d :: String -> Bool
d [] = False
d (x:xs)     | not (length xs == 0), x == head xs   =  True 
             | not (length xs == 0), x /= head xs   = d xs
             |otherwise                      = False

-- c

prop_cd :: String -> Bool
prop_cd xs =
    c xs == d xs
