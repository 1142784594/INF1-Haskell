-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

{-# LANGUAGE TemplateHaskell #-}
module Tutorial6 where
  
import Data.List (nub)
import Control.Monad (liftM, liftM2)
import Test.QuickCheck (quickCheck, quickCheckAll,
                        Arbitrary(arbitrary), oneof, sized)
  
-- ** Implementing propositional logic in Haskell

-- The datatype 'Wff' a
data Wff a = V a
           | F
           | T
           | Not (Wff a)
           | Wff a :|: Wff a
           | Wff a :&: Wff a
           | Wff a :->: Wff a
           | Wff a :<->: Wff a
           deriving (Eq)

-- we will use these as propositional letters in examples
data Atom = A|B|C|D|P|Q|R|S|W|X|Y|Z
           deriving (Eq, Show)

-- Show
par :: String -> String
par s  =  "(" ++ s ++ ")"

showWff :: (Show a) => Wff a -> String
showWff (V x)        =  show x
showWff F            =  "0"
showWff T            =  "1"
showWff (Not p)      =  par ("~" ++ showWff p)
showWff (p :|: q)    =  par (showWff p ++ "|" ++ showWff q)
showWff (p :&: q)    =  par (showWff p ++ "&" ++ showWff q)
showWff (p :->: q)    =  par (showWff p ++ "->" ++ showWff q)
showWff (p :<->: q)    =  par (showWff p ++ "<->" ++ showWff q)


-- Environments and lookup
type Env a = [(a, Bool)]

lookUp :: Eq a => Env a -> a -> Bool
lookUp env x = the [ b | (y, b) <- env, x == y ]
    where the [b] = b
          the []  = error ("valuation undefined")
          the bs  = error ("multiple values")

-- evaluates a wff in a given environment
eval :: Eq a => Env a -> Wff a -> Bool
eval e (V x)       = lookUp e x
eval e F           = False
eval e T           = True
eval e (Not p)     = not (eval e p)
eval e (p :|: q)   = eval e p || eval e q
eval e (p :&: q)   = eval e p && eval e q
eval e (p :->: q)  = (not (eval e p ))|| eval e q
eval e (p :<->: q) = eval e p == eval e q 

-- list the atoms that occur in a wff
-- atoms in the result must be unique
atoms :: Eq a => Wff a -> [a]
atoms (V x)       = [x]
atoms (F)         = []
atoms (T)         = []
atoms (Not p)     = atoms p
atoms (p :|: q)   = nub (atoms p ++ atoms q)
atoms (p :&: q)   = nub (atoms p ++ atoms q)
atoms (p :->: q)   = nub (atoms p ++ atoms q)
atoms (p :<->: q)   = nub (atoms p ++ atoms q) 

-- creates all possible truth assignments for a set of atoms
envs :: [a] -> [Env a]
envs []     = [[]]
envs (x:xs) = [ (x,False):e | e <- envs xs ] ++
              [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Eq a => Wff a -> Bool
satisfiable p = or [ eval e p | e <- envs (atoms p) ]

-- ** Exercises

-- 1.
wff1, wff2, wff3 :: Wff Atom
wff1 = V P :|: (Not (V P))
wff2 = ((V P) :|: V Q) :&: ((V P) :&: (V Q))
wff3 = (V P:&:(V Q:|:V R)):&:(((Not (V P)):|:Not(V Q)):&:(Not(V P):|:Not(V R)))

{-
-- table wff1
P | (P|(~P))
- | --------
0 |    1
1 |    1
-- table wff2
P Q | ((P|Q)&(P&Q))
- - | -------------
0 0 |       0
0 1 |       0
1 0 |       0
1 1 |       1
-- table wff3
P Q R | ((P&(Q|R))&(((~P)|(~Q))&((~P)|(~R))))
- - - | -------------------------------------
0 0 0 |                   0
0 0 1 |                   0
0 1 0 |                   0
0 1 1 |                   0
1 0 0 |                   0
1 0 1 |                   0
1 1 0 |                   0
1 1 1 |                   0
-- satisfiable wff1
True
-- satisfiable wff2
True
-- satisfiable wff3
False
-}

-- 2. 
tautology :: Eq a => Wff a -> Bool
tautology p = and [eval q p| q<-envs (atoms p)]

prop_taut :: Wff Atom -> Bool
prop_taut p = 
  tautology p == not (satisfiable (Not p))

{-
-- tautology wff1
True
--tautology wff2
False
--tautology wff3
False
-}


-- 3.
wff4, wff5, wff6 :: Wff Atom
wff4 = ((V P):->:(V Q)):<->:((Not(V P)):|:(V Q))
wff5 = ((V P):->:(V Q)):&:((V Q):->:(V P))
wff6 = ((V P):->:(V Q)):&:((V Q):->:(V R)):&:(Not((V P):->:(V R)))

{-
--*Tutorial6>  table (V P :->: V Q)
--P Q | (P->Q)
- - | ------
0 0 |   1   
0 1 |   1   
1 0 |   0   
1 1 |   1 
--*Tutorial6> table (V P :<->: V Q)
P Q | (P<->Q)
- - | -------
0 0 |    1
0 1 |    0
1 0 |    0
1 1 |    1
-- table wff4
P Q | ((P->Q)<->((~P)|Q))
- - | -------------------
0 0 |          1
0 1 |          1
1 0 |          1
1 1 |          1
--table wff5
P Q | ((P->Q)&(Q->P))
- - | ---------------
0 0 |        1
0 1 |        0
1 0 |        0
1 1 |        1
--table wff6
P Q R | (((P->Q)&(Q->R))&(~(P->R)))
- - - | ---------------------------
0 0 0 |              0
0 0 1 |              0
0 1 0 |              0
0 1 1 |              0
1 0 0 |              0
1 0 1 |              0
1 1 0 |              0
1 1 1 |              0
-- satisfiable wff4
True
-- satisfiable wff5
True
-- satisfiable wff6
False
-- tautology wff4
True
-- tautology wff5
False
-- tautology wff6
False
-}


-- ** Optional Material

-- 5.
isNNF :: Wff a -> Bool
isNNF (V x)       =  True
isNNF F           = True
isNNF T           = True 
isNNF (Not (V x)) = True
isNNF (Not p)     = False 
isNNF (p :|: q)   = isNNF p && isNNF q 
isNNF (p :&: q)   = isNNF p && isNNF q 
isNNF (p :->: q)  = False 
isNNF (p :<->: q)  = False

-- 6.
impElim :: Wff a -> Wff a
impElim (V x)       = V x
impElim (Not p)     =  Not ( impElim p)
impElim (p :->: q)  =  (impElim (Not p ):|:(impElim q))
impElim (p :<->: q) = (impElim (Not p) :|: impElim q) :&: (impElim (Not q) :|: impElim p)
impElim (p :|: q)   =  (impElim p :|: impElim q)
impElim (p :&: q )  = (impElim p :&: impElim q)
impElim F           = F
impElim T           = T

--7.
notElim :: Wff a -> Wff a
notElim (V x)             =  (V x)
notElim F                 =  F
notElim T                 =  T
notElim (Not (V x))       =  (Not (V x))
notElim (Not (Not p))     =  notElim p
notElim (p :|: q)         =  (notElim p) :|: (notElim q)
notElim (p :&: q )        =  (notElim p) :&: (notElim q)
notElim (Not (p:|:q))     =  (notElim (Not p)):&:(notElim (Not q))
notElim (Not (p:&:q))     =  (notElim (Not p)) :|: (notElim (Not q))
notElim (Not F)           = T
notElim (Not T)           = F

--8.
--8.
toNNF :: Wff a -> Wff a
toNNF p =  notElim (impElim p) 

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff Atom -> Bool
prop_NNF1 p = isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Wff Atom -> Bool
prop_NNF2 p = equivalent p (toNNF p)

-- ** Challenge

-- 9.
isCNF :: Wff a -> Bool
isCNF =  undefined

-- 10.
fromLists :: [[Wff a]] -> Wff a
fromLists =  undefined

-- 11.
toLists :: Wff a -> [[Wff a]]
toLists = undefined

-- 12.
toCNF :: Wff a -> Wff a
toCNF = undefined

-- check if result of toCNF is in CNF
prop_CNF1 :: Wff Atom -> Bool
prop_CNF1 f = isCNF (toCNF f)

-- check if result of toCNF is equivalent to its input
prop_CNF2 :: Wff Atom -> Bool
prop_CNF2 p = equivalent p (toCNF p)


-- ** Drawing Tables

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s = replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s = replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False = "0"
fort True  = "1"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab =
  putStr (
    unlines [ unwords (zipWith centre widths row) | row <- tab ]
  )
  where
    widths  = map length (head tab)

table :: (Eq a, Show a) => Wff a -> IO ()
table p = tables [p]

tables :: (Eq a, Show a) => [Wff a] -> IO ()
tables ps  =
  let xs = nub (concatMap atoms ps) in
   showTable (
     [ map show xs ++ ["|"] ++ [show p | p <- ps]           ] ++
     [ dashvars xs ++ ["|"] ++ [dash (show p) | p <- ps ]   ] ++
     [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
     )
  where  dashvars xs   = [ dash (show x) | x <- xs ]
         evalvars e xs = [ fort (eval e (V x)) | x <- xs ]


-- ** For QuickCheck
 
equivalent :: (Eq a) => Wff a -> Wff a -> Bool
equivalent p q  =  tautology ((p :&: q) :|: (Not p :&: Not q))

instance Show a => Show (Wff a) where
  show = showWff

instance Arbitrary Atom where
  arbitrary = oneof (map return [ A, B, C, D, P, Q, R, S, W, X, Y, Z])

instance Arbitrary a => Arbitrary (Wff a) where
  arbitrary = sized wff
      where
        wff n | n <= 0    = liftM V arbitrary
              | otherwise = oneof [ liftM V arbitrary
                                  , liftM Not wff2
                                  , liftM2 (:|:) wff2 wff2
                                  , liftM2 (:&:) wff2 wff2
                                  , liftM2 (:->:) wff2 wff2
                                  , liftM2 (:<->:) wff2 wff2
                                  ]
               where
                 wff2  =  wff (n `div` 4)
