module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Distribution.Simple.Utils (xargs)


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

-- 2.
intersperse :: a -> [a] -> [a]
intersperse n [] = [n]
intersperse n (x:xs) = n :x : intersperse n xs

-- 3.
showRow :: String -> String
showRow xs = foldr (++) [] (intersperse "|" (groupBy 3 xs))

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid xs =  help1 ["-------------"] (map (showRow) xs )

help1 :: [String] -> [String] -> [String]
help1 n []     = n
help1 n xs = n ++ take 3 xs  ++  help1 n (drop 3 xs ) 

-- 5.
put :: Matrix Digit -> IO ()
put = putStrLn.unlines.showGrid

-- 6.
showMat :: Matrix Digit -> String
showMat  = help2.(foldr (++) []) 

help2 :: String -> String 
help2 []     = []
help2 (x:xs) |  x == ' '  = "." ++ help2 xs
             | otherwise = x: help2 xs

readMat :: String -> Matrix Digit
readMat [] = []
readMat xs = take 9 (help3 xs) : readMat (drop 9 (help3 xs))   

help3 :: String -> String 
help3 []     = []
help3 (x:xs) |  x == '.'  = ' ' : help3 xs
             | otherwise = x: help3 xs

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices []       = [] 
choices  (x:xs)  = map (help4) x : choices xs   

help4 :: Digit -> String
help4 x | x== ' '   = "123456789"
        |otherwise  = [x]


-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand x = cp (map (cp) x) 

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = 
        length (expand m) == (length [ y | y <- [ x | x<-m]]) ^ ((length[p | p<-m])^2) 


-- 10.
easySols :: Integer
easySols  = fromIntegral (length (expand (choices easy)))

--maybe 9^81, which is 10^60 more than the universe.

-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows c = c 
cols c = transpose c
boxs  = ungroup.map (map ungroup.transpose).group.map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = nub [ y | y<-xs ] == xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = and [ distinct y | y <- rows g ] && and [ distinct x | x <- cols g ] && and [distinct b | b<- boxs g]

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

--No.

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = [ help7 p (help5 row)| p <- row ]
 
help5 :: Row [Digit] -> Row [Digit]
help5 [] = []
help5 (x:xs) | x == "1"    = x: help5 xs
             | x == "2"    = x: help5 xs
             | x == "3"    = x: help5 xs
             | x == "4"    = x: help5 xs
             | x == "5"    = x: help5 xs
             | x == "6"    = x: help5 xs
             | x == "7"    = x: help5 xs
             | x == "8"    = x: help5 xs
             | x == "9"    = x: help5 xs
             | otherwise   = help5 xs

help6 :: [Digit] -> Row [Digit] -> [Digit]
help6 [] c = []
help6 (x:xs) c | x `elem` concat [ q | q<- c ]    =  help6 xs c
               | otherwise                        =  x : help6 xs c       

help7 :: [Digit] -> Row [Digit] -> [Digit]
help7 xs c | length xs == 1 = xs
           |  otherwise      = help6 xs c

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f. map pruneRow. f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs.pruneBy cols. pruneBy rows

-- 19.
many :: Eq a => (a -> a) -> a -> a
many g x | g x /= x = many g (g x)
         | otherwise  = g x  

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract = map (map the)

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve c = extract (many prune (choices c))

--easy and medium.

-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed mat = and [null c | c <- [ x | x <-  mat]]

-- 23.
solved :: Matrix [Digit] -> Bool
solved = all (all ((==1). length))

-- 24.
shortest :: Matrix [Digit] -> Int
shortest [] = 0
shortest (x:xs) |  or [length p > 1| p<- x]  = minimum [ length p | p <- x, length p > 1]
                | otherwise                  = shortest xs
-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat =  [(fst (break (any (\ds -> length ds == shortest mat )) mat)) 
               ++ [(fst (break (\ds -> length ds == shortest mat ) (head (snd (break (any (\ds -> length ds == shortest mat )) mat)))))++ c] 
               ++ (tail (snd (break (any (\ds -> length ds == shortest mat )) mat))) | c<- (help8 (snd (break (\ds -> length ds == shortest mat ) (head (snd (break (any (\ds -> length ds == shortest mat )) mat))))))]

help8 :: [[Digit]] -> [[[Digit]]]
help8 []      = []
help8 ([]:xs) = []
help8 (x:xs)  = ([head x ] : xs) : help8 (tail x :xs)

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search c =[p | p<-(map (extract) (help9 (choices c))), valid p]

help9 :: Matrix [Digit] -> [Matrix [Digit]] 
help9 c | failed (many prune c) = []
        | solved (many prune c) = [many prune c]
        | otherwise             = concatMap help9 (expand1 (many prune c))
-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil
