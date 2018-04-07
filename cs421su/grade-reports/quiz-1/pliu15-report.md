Quiz 1 - pliu15
===============

Score: 20/50

Problem: `dropWhile`
--------------------

### Submitted Code

``` {.haskell}
-- Your code goes here.
mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile 0 list  = list
mydropWhile _ [] = []
mydropWhile n (x:xs) 
    | x < n = mydropWhile (n-1) xs
    | otherwise = x:xs
--natList n (x:xs) 
--    | x < n = 0:(x+1):natList xs
--    | otherwise = x:xs
--mydrop :: int -> [a] -> [a]
```

Passed!

Problem: `natList`
------------------

### Submitted Code

``` {.haskell}
-- Your code goes here.
natList :: Int -> [Int]
natList 0 = []
natList n < 0 = []
natList n (x:xs) 
    | x < n = 0:(x+1):natList xs
    | otherwise = x:xs



--inclist :: (Num a) => [a] -> [a]
--inclist [] = []
--inclist (x:xs) = (x+1):inclist xs

--natList :: [Integer]
--natList = 0:inclist nats
```

### (-10) compile

You have syntax errors preventing your code from compiling.

Problem: `mulList`
------------------

### Submitted Code

``` {.haskell}
-- Your code goes here.
--mulList :: [Int] -> Int -> [Int]
--mulList list1 list2 = sumzip (myzip list1 list2)
--    where sumzip [] = []
--        sumzip ((a,b):xs) = a+b:sumzip xs
        
--myzip :: [a] => [a] -> [b] -> [(a,b)]
--myzip list [] = []
--myzip [] list = []
--myzip (x:xs) (y:ys) = (x,y):myzip xs ys

mulList :: [Int] -> Int -> [Int]
mulList [] n = []
mulList List 0 = []
mulList (x:xs) n = (x*n):mulList xs


--mulList (x:xs) (y:ys) = (x*y):mulLists xs ys
```

### (-10) compile

mulList was applied to too few arguments, causing a compile error.

Problem: `prodlist`
-------------------

### Submitted Code

``` {.haskell}
module Submission where

-- Your code goes here.
prodlist :: (Num a) => [a] -> a
prodlist xs = foldl1 (*) xs
--prodlist xs = foldl (*) 1 (map prodlist xs)
```

Passed!

Problem: `Tree-sumLeaves_twiceTree`
-----------------------------------

### Submitted Code

``` {.haskell}
data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
  deriving Show

sumLeaves :: (Num a) => Tree a -> a
sumLeaves Leaf = 0
sumLeaves (Node n a b) = n + (sumLeaves a) + (sumLeaves b)


twiceLeaves :: (Num a) => Tree a -> Tree a
twiceLeaves Leaf = 0
twiceLeaves (Node n a b)
    | Leaf = 2 * (twiceLeaves a)
--twiceLeaves (Node n a b) = 2* (twiceLeaves Leaf a)
--map (*2)
```

### (-10) type

Your code will not compile due to syntax or type errors.