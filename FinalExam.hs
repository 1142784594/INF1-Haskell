module FinalExam where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1
ok :: String -> Bool 
ok c = and [ isLower q | q <- c] && length c < 6
{-
*FinalExam> ok "a"
True
*FinalExam> ok "zzzzz"
True
*FinalExam> ok "Short"
False
*FinalExam> ok "longer"
False
*FinalExam> ok "???"
False
--}
-- 1a

f :: [String] -> String
f c = head ([ p | p<-c , ok p , and [p <= x | x<-c, ok x] ] ++ ["zzzzz"])

{-
*FinalExam> f ["a","bb","ccc","dddd","eeeee","ffffff"]
"a"
*FinalExam> f ["uuuuuu","vvvvv","wwww","xxx","yy","z"]
"vvvvv"
*FinalExam> f ["Short","longer","???"]
"zzzzz"
--}

-- 1b

g :: [String] -> String
g []   = "zzzzz"
g (x:xs)  | ok x , x == min x (g xs)             = x
          | otherwise                            = g xs

{-
*FinalExam> g ["a","bb","ccc","dddd","eeeee","ffffff"]
"a"
*FinalExam> g ["uuuuuu","vvvvv","wwww","xxx","yy","z"]
"vvvvv"
*FinalExam> g ["Short","longer","???"]
"zzzzz"
--}

-- 1c

h :: [String] -> String
h c = foldr min "zzzzz" (filter ok c)

prop_f_g_h :: [String] -> Bool
prop_f_g_h c = 
  f c == g c && g c == h c 
{-
*FinalExam> quickCheck prop_f_g_h
+++ OK, passed 100 tests.
*FinalExam> quickCheck prop_f_g_h
+++ OK, passed 100 tests.
--}
-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i a b = tail a ++ [head b]
{-
*FinalExam> i "abc" "def"
"bcd"
*FinalExam> i "def" "ghi"
"efg"
*FinalExam> i "ghi" "abc"
"hia"
--}
-- 2b

j :: [[a]] -> [[a]]
j c = [i a b | (a,b)<- zip c (tail c ++ [head c])]

{-
*FinalExam> j ["abc","def","ghi"]
["bcd","efg","hia"]
*FinalExam> j ["abc","def","ghi"]
["bcd","efg","hia"]
*FinalExam> j ["a","b","c"]
["b","c","a"]
*FinalExam> j ["a"]
["a"]
--}

-- 2c

k :: [[a]] -> [[a]]
k []   = []
k x@(y:ys) = help1 (help2 x y) 

help1 :: [[a]] -> [[a]]
help1 [] = []
help1 [x] = [] 
help1 (a:b:xs) = i a b : help1 (b:xs)

help2 :: [a]->a->[a]
help2 []  c = [c]  
help2 (x:xs) c = x: (help2 xs c)  
{-
*FinalExam> k ["abc","def","ghi"]
["bcd","efg","hia"]
*FinalExam> k ["abc","def","ghi"]
["bcd","efg","hia"]
*FinalExam> k ["a","b","c"]
["b","c","a"]
*FinalExam> k ["a"]
["a"]
--}

-- Question 3

data Wff = X
         | Y
         | Tr
         | Fa
         | Not Wff
         | Wff :&: Wff
         | Wff :|: Wff
         | Wff :->: Wff
  deriving (Eq, Show)

instance Arbitrary Wff where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return Tr,
              return Fa ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return Tr,
              return Fa,
              liftM Not wff,
              liftM2 (:&:) wff wff,
              liftM2 (:|:) wff wff,
              liftM2 (:->:) wff wff]
      where
      wff = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Wff -> Bool
eval a b X             = a
eval a b Y             = b 
eval a b Tr            = True 
eval a b Fa            = False 
eval a b (Not c)       = not (eval a b c)
eval a b (p :|: q)     = (eval a b p ) || (eval a b q) 
eval a b (p :&: q)     = (eval a b p)  && (eval a b q)
eval a b (p :->: q)    = not (eval a b p ) || (eval a b q)

{-
*FinalExam> eval False False ((X :->: Y) :&: (Not Y :|: X))
True
*FinalExam> eval False True ((X :->: Y) :&: (Not Y :|: X))
False
*FinalExam> eval True False ((X :->: Y) :&: (Not Y :|: X))
False
*FinalExam> eval True True ((X :->: Y) :&: (Not Y :|: X))
True
--}

-- 3b

simple :: Wff -> Bool
simple Tr = True 
simple Fa = True
simple X  = True 
simple Y  = True 
simple (Not p) | p `elem` [Tr, Fa] = False 
               | otherwise         = simple p
simple (p :|: q ) | p `elem` [Tr,Fa] ||  q `elem` [Tr,Fa]= False 
                  | otherwise                            = simple p && simple q
simple (p :&: q ) | p `elem` [Tr,Fa] ||  q `elem` [Tr,Fa]= False 
                  | otherwise                            = simple p && simple q                
simple (p :->: q ) | p `elem` [Tr,Fa] ||  q `elem` [Tr,Fa]= False 
                  | otherwise                            = simple p && simple q

{-
*FinalExam> simple Tr
True
*FinalExam> simple Fa
True
*FinalExam> simple ((Tr :|: X) :->: (Tr :&: Y))
False
*FinalExam> simple ((X :|: Fa) :->: (Y :&: Fa))
False
*FinalExam> simple ((X :&: Y) :->: (X :|: Y))
True
--}


-- 3c

simplify :: Wff -> Wff
simplify X = X
simplify Y = Y
simplify Tr = Tr
simplify Fa = Fa
simplify (Not Tr) = Fa
simplify (Not Fa) = Tr
simplify (Fa :&: p) = Fa
simplify (p :&: Fa) = Fa
simplify (Tr :&: p) = p
simplify (p :&: Tr) = p
simplify (Fa :|: p) = p
simplify ( p :|: Fa) = p
simplify (Tr :|: p) = Tr
simplify ( p :|: Tr) = Tr
simplify ( Fa :->: p) = Tr
simplify ( p :->: Tr) = Tr
simplify ( Tr :->: p) = p
simplify (p :->: Fa) = Not p
simplify(p :->: q)  |simple (p :->: q) = p :->: q
                    |otherwise =  simplify(simplify p :->: simplify q)
simplify (p :|: q)  |simple (p :|: q) = p :|: q
                    |otherwise =  simplify(simplify p :|: simplify q)
simplify (p :&: q ) |simple (p :&: q) = p :&: q
                    |otherwise =  simplify(simplify p :&: simplify q)
simplify c = c

{-
*FinalExam> simplify Tr
Tr
*FinalExam> simplify Fa
Fa
*FinalExam> simplify ((Tr :|: X) :->: (Tr :&: Y))
Y
*FinalExam> simplify ((X :|: Fa) :->: (X :&: Fa))
Not X
*FinalExam> simplify ((X :|: Y) :->: (X :&: Y))
(X :|: Y) :->: (X :&: Y)
--}