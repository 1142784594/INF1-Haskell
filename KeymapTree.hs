module KeymapTree ( Keymap,
                    invariant, keys,
                    size, depth, get, set, del, select,
                    toList, fromList,
                    filterLT, filterGT, merge,
                  )
where

-- Modules for testing
  
import Test.QuickCheck
import Control.Monad
import Data.List
  
-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)
                deriving (Eq, Show)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Invariant

invariant :: Ord k => Keymap k a -> Bool
invariant Leaf  =  True
invariant (Node k _ left right)  =  all (< k) (keys left) &&
                                    all (> k) (keys right) &&
                                    invariant left &&
                                    invariant right

keys :: Keymap k a -> [k]
keys Leaf  =  []
keys (Node k _ left right)  =  keys left ++ [k] ++ keys right

-- Exercise 3

size :: Keymap k a -> Int
size  (Node k _  left right)                         = 1+ size (left) + size (right)
size  (Leaf)                                         = 0


depth :: Keymap k a -> Int
depth (Leaf)                  =  0
depth (Node k _  left right)  =  1 + (max (depth (left)) (depth (right)) )

-- Exercise 4

toList :: Ord k => Keymap k a -> [(k,a)]
toList (Leaf)                 = []
toList (Node k a  left right) =  toList(left) ++ [(k,a)] ++ toList(right)



-- Exercise 5

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node key value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)
                                     

-- Exercise 6

get :: Ord k => k -> Keymap k a -> Maybe a
get c xs =  lookup c (toList xs)

prop_set_get :: Int -> String -> Keymap Int String -> Bool
prop_set_get k v db = get k (set k v db) == Just v

-- Exercise 7

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList  = foldr (uncurry (set)) Leaf

prop_toList_fromList :: [Int] -> [String] -> Bool
prop_toList_fromList xs ys  =  toList (fromList zs) == sort zs
  where zs = zip (nub xs) ys


-- ** Optional Material

-- Exercise 9

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT k c =  fromList [(fst p, snd p) | p<- (toList c) , (fst p) < k ]
                                     
filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT k c =  fromList [(fst p, snd p) | p<- (toList c) , (fst p) > k ]

-- Exercise 10
                                     
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge c a = foldr (uncurry (set)) Leaf ((toList c) ++ (toList a))

merge1 :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge1 Leaf  tree2 = tree2
merge1 tree1 Leaf  = tree1
merge1 (Node k1 v1 leftfst rightfst) (Node k2 v2 leftsnd rightsnd)
      | k1 == k2 = Node k1 v1 
                   (merge1 leftfst leftsnd) (merge1 rightfst rightsnd)
      | k1 >  k2 = merge1 (Node k1 v1 
                         (merge1 leftfst (Node k2 v2 leftsnd Leaf)) rightfst )  
                          rightsnd
      | k1 <  k2 = merge1 (Node k1 v1 
                          leftfst (merge1 rightfst (Node k2 v2 Leaf rightsnd)))
                          leftsnd

prop_merge :: Ord k => Eq a => Keymap k a -> Keymap k a -> Bool
prop_merge k a =
  merge k a == merge1 k a               
-- Exercise 11

del :: Ord k => k -> Keymap k a -> Keymap k a
del k c = fromList [(fst a, snd a)| a <- (toList c), (fst a) /= k ]  

-- Exercise 12

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select p c = fromList [ q | q<-(toList c), p (snd q)  ]   

-- Instances for QuickCheck -----------------------------

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList (liftM2 zip (liftM nub arbitrary) arbitrary)
