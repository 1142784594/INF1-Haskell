module Tutorial8 where  

import System.Random
import Test.QuickCheck
import Data.Maybe

-- Importing the keymap module

import KeymapList
--import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item

-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Exercise 1

getItems :: [Barcode] -> Catalogue -> [Item]
getItems  c xs= catMaybes [ get p xs | p<-c]

-- Exercise 2

{-
*Tutorial8> db <- readDB
Done
(4.12 secs, 2,274,701,600 bytes)
*Tutorial8> size db
104651
(0.02 secs, 70,280 bytes)
*Tutorial8> ks <- samples 3 db
(0.05 secs, 7,635,416 bytes)
*Tutorial8> force (getItems ks db)
()
(0.05 secs, 68,072 bytes)

If the database was two times bigger,
how would you expect the time to change?
I think the time will be twice of the original one.
-}

-- for Exercises 3--7 check KeymapTree.hs 

-- Exercise 8

{-
*Tutorial8> db <- readDB
Done
(8.56 secs, 3,361,110,584 bytes)
*Tutorial8> size db
104651
(0.09 secs, 27,698,176 bytes)
*Tutorial8> depth db
40
(0.10 secs, 26,858,024 bytes)
*Tutorial8> ks <- loadKeys
(0.00 secs, 85,136 bytes)
*Tutorial8> force (getItems ks db)
()
(0.17 secs, 143,893,288 bytes)

If the database was two times bigger,
how would you expect the time to change?
I think the time will be log2n/logn.
-}

-- for Exercises 9--12 check KeymapTree.hs 

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine (lines dbl))
            putStrLn (force (show db) `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

samples :: Int -> Catalogue -> IO [Barcode]
samples n db =
  do g <- newStdGen
     let allKeys = [ key | (key,item) <- toList db ]
     let indices = randomRs (0, length allKeys - 1) g
     let keys = take n [ allKeys !! i | i <- indices ]
     saveKeys keys
     return (force keys `seq` keys)

saveKeys :: [Barcode] -> IO ()
saveKeys = writeFile "keys.cache" . show

loadKeys :: IO [Barcode]
loadKeys = do
  keys <- read <$> readFile "keys.cache"
  return (force keys `seq` keys)

force :: [a] -> ()
force = foldr seq ()
