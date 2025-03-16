module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck



-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Ceasar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp z xs | z `elem` [ fst x | x<-xs ]= head [snd x | x<-xs, fst x ==z ]
            | otherwise = z

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec z [] = z
lookUpRec z ((x,y) :rest)                              | x == z              = y
                                                    |otherwise            = lookUp z rest

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp z xs=
    lookUp z xs == lookUpRec z xs

-- 2.
encipher :: Int -> Char -> Char
encipher k x = lookUp x (makeKey k)

-- 3.
normalise :: String -> String
normalise str = [toUpper x | x<-str, isLetter x ]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (x:xs)    | isLetter x  = (toUpper x : normaliseRec xs)
                       | otherwise   = normaliseRec xs

prop_normalise :: String -> Bool
prop_normalise str=
      normalise str ==  normaliseRec str

-- 4.
enciphers :: Int -> String -> String
enciphers k str = [encipher k x | x <- (normalise str)]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(snd x, fst x) | x<-xs ]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x,y) :rest) = (y,x): reverseKeyRec rest

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey  xs =
    reverseKey xs == reverseKeyRec xs

-- 6.
decipher :: Int -> Char -> Char
decipher z x = head [c | isUpper x , (c, d)<-makeKey z , d == x ]

decipherStr :: Int -> String -> String
decipherStr z [] = []
decipherStr z (x:xs)  | isUpper x = head [c | isUpper x , (c, d)<-makeKey z , d == x ] : decipherStr z xs
                      | otherwise = decipherStr z xs


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates str = [(z,decipherStr z str) | z<-[0..26], "THE" `isInfixOf` decipherStr z str || "AND" `isInfixOf` decipherStr z str ]

-- 8.
splitEachFive :: String -> [String]
splitEachFive [] = ["XXXXX"]
splitEachFive str  | length str >= 5 = take 5 str : splitEachFive (drop 5 str)
                   | otherwise       = [take 5  (str ++ "XXXXXX")]

prop_transpose :: String -> Bool
prop_transpose str =
     splitEachFive str == transpose (transpose (splitEachFive str))

-- 9.
encrypt :: Int -> String -> String
encrypt z str = concat (transpose (splitEachFive (enciphers z str)))

-- 10.
undo :: Int ->String -> [String]
undo i str | null str       = []
           | otherwise      = take i str: undo i (drop i  str) 

decrypt :: Int -> String -> String
decrypt z str = decipherStr z (concat(transpose (undo (div (length str) 5)  str)))