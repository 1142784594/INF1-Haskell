module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck


-- ** Optional material

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs = [ 2*x | x<-xs] 

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs)=  x*2 : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO  xs = map db xs
        where 
        db x = x*2

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs =  
    (doublesComp xs == doublesRec xs) && (doublesRec xs  == doublesHO  xs)

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp b xs =  [ x | x<-xs, x > 2 ]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec b [] = []
abovesRec b (x:xs) | x > b       = x : abovesRec b xs
                    | otherwise   = abovesRec b xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO b xs =  filter ( > b) xs 

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves b xs =  
    abovesComp b xs == abovesRec b xs && abovesRec b xs == abovesHO b xs

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b   | a==b       = False 
          | otherwise  = True


-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO  [] = True 
parityHO  xs   =  foldl  (xor)  True xs 

-- d.
prop_parity :: [Bool] -> Bool
prop_parity xs =  
   parityRec xs == parityHO  xs
-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp xs =  and [isUpper x | x<- xs, isLetter x]

-- b.
allcapsRec :: String -> Bool
allcapsRec []  = True
allcapsRec (x:xs) | isLetter x = isUpper x && allcapsRec xs
                  | otherwise  = allcapsRec xs 

-- c.
allcapsHO :: String -> Bool
allcapsHO xs = foldr (&&) True (map isUpper (filter (isLetter) xs))

-- d.
prop_allcaps :: String -> Bool
prop_allcaps xs = 
     allcapsComp xs == allcapsRec xs && allcapsRec xs == allcapsHO xs


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) =  all (== x) xs

-- b.
valid :: Matrix -> Bool
valid [] = False 
valid (x:xs) = uniform (length x:[length xs])


-- 6.
width :: Matrix -> Int
width m = length (head m) 

height :: Matrix -> Int
height m = length (head(transpose m))

plusRow :: [Rational] -> [Rational] -> [Rational]
plusRow [] [] = []
plusRow (a:as) (b:bs) = a+b : plusRow as bs

plusM :: Matrix -> Matrix -> Matrix
plusM (x:xs) (y:ys) = zipWith plusRow (x:xs) (y:ys)

-- 7.
dot :: [Rational] -> [Rational] -> Rational
dot [x] [y] = x*y 
dot  (x:xs) (y:ys) = x*y + dot xs ys
    

timesM :: Matrix -> Matrix -> Matrix
timesM [] ys = []
timesM (x:xs) ys = [map (dot x) (transpose ys)] ++ (timesM xs ys)


-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [uncurry f (x,y)| (x,y)<- (zip xs ys)]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- ** Challenge

-- 9.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
