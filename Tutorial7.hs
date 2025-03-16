module Tutorial7 where

import LSystem
import Test.QuickCheck

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy 0 c = Sit  
copy n c = c :#: (copy (n-1) c)

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon x n = copy n (Go x :#: Turn (360/fromIntegral n))

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n 
     where 
         f 0 = Go 10
         f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
         n = Turn 60
         p = Turn (-60)

         

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski x = f x 
  where 
      f 0 = GrabPen red :#: Go 10
      f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x -1) :#: n :#: g (x-1) :#: n :#: f (x-1) 
      n = Turn 60
      p = Turn (-60)
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert  x = l x
   where
    f 0     = GrabPen red :#: Go 10
    f x     = f (x-1)
    l 0     = GrabPen green :#: Go 10
    l x     = p :#: r (x-1) :#: f (x-1) :#: n :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: n :#: f (x -1) :#: r (x-1) :#: p
    r 0     = GrabPen blue :#: Go 10
    r x     = n :#: l (x -1) :#: f (x -1) :#: p :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: p :#: f (x-1) :#: l (x-1) :#: n 
    n       = Turn 90
    p       = Turn (-90)


-- 5. dragon
dragon :: Int -> Command
dragon x = l x
   where 
       f 0 = GrabPen red :#: Go 10
       f x = f (x-1)
       l 0 = GrabPen green :#: Go 10
       l x = l (x-1) :#: p :#: r (x-1) :#: f (x - 1) :#: p
       r 0 = GrabPen blue :#: Go 10
       r x = n :#: f (x-1) :#: l (x-1) :#: n :#: r (x-1)
       n   = Turn 90
       p   = Turn (-90)

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (Sit)     = []
split (a :#: b) = split a ++ split b
split (x)       = [x]
             

-- 6b. join
join :: [Command] -> Command
join []       = Sit
join (x:xs)   = x :#: join xs

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent a b =
    split a == split b  

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = 
    equivalent c (join (split c)) 

help1 :: Command -> Bool
help1 (a :#: b) = False
help1 (Sit)     = False
help1 x         = True


prop_split :: Command -> Bool
prop_split c = and (map (help1) (split c))

--7. optimise
optimise :: Command -> Command
optimise c = help5 (join (help2 (split c)))


help2 :: [Command] -> [Command]
help2   = help4. help3


help3 :: [Command] -> [Command]
help3 []             = []
help3 (Sit :xs)      = help2 xs
help3 (Go 0 :xs)     = help2 xs
help3 (Turn 0 :xs)   = help2 xs
help3 (x:xs)         = x : help3 xs

help4 :: [Command] -> [Command]
help4 []                     =[]
help4 (Go a : Go b : xs)     = help2 (Go (a+b) : xs)
help4 (Turn a : Turn b :xs)  = help2 (Turn (a+b): xs)
help4 (x:xs)                 = x : help4 xs

help5 :: Command -> Command
help5 (a :#: Sit) = help5 a
help5 (a :#: b )  = help5 a :#: help5 b
help5 a           = a







