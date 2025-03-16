module Wff where

import Control.Monad
import Parser
import Test.QuickCheck

data Wff   = X
           | Y
           | Tr
           | Fa
           | Not Wff
           | Wff :&: Wff
           | Wff :|: Wff
  deriving (Eq)

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
              liftM2 (:|:) wff wff]
      where
      wff = gen (n `div` 2)

par :: String -> String
par s  =  "(" ++ s ++ ")"

instance Show Wff where
  show X            =  "X"
  show Y            =  "Y"
  show Fa           =  "0"
  show Tr           =  "1"
  show (Not p)      =  par ("~" ++ show p)
  show (p :|: q)    =  par (show p ++ "|" ++ show q)
  show (p :&: q)    =  par (show p ++ "&" ++ show q)


-- ** Optional Exercises **

-- Exercise 1

parseWff :: Parser Wff
parseWff =  parseX `mplus` parseY `mplus` parseFa `mplus` parseTr `mplus` parseNot `mplus` parseAnd `mplus` parseOr


parseX :: Parser Wff
parseX = do  token 'X'
             return X
           
parseY :: Parser Wff           
parseY = do  token 'Y'
             return Y            

parseFa :: Parser Wff          
parseFa = do token '0' 
             return Fa

parseTr :: Parser Wff
parseTr = do token '1' 
             return Tr

parseNot :: Parser Wff
parseNot = do token '(' 
              token '~'
              e <- parseWff
              token ')'
              return (Not e)

parseAnd :: Parser Wff
parseAnd = do token '(' ;
              f <- parseWff;
              token '&' ;
              e <- parseWff;
              token ')' ;
              return (f :&: e) 

parseOr :: Parser Wff
parseOr = do token '(' ;
             f <- parseWff;
             token '|' ;
             e <- parseWff;
             token ')' ;
             return (f :|: e)
                    
          
-- Exercise 2

prop_roundtrip :: Wff -> Bool
prop_roundtrip c=  
  parse parseWff (show c) == c 

