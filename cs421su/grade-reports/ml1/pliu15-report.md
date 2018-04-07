ML1 - pliu15
============

Score: 23/25

Problem: `cons2list`
--------------------

### Submitted Code

``` {.haskell}
data Cons a = Cons a (Cons a)
            | Nil
  deriving (Show,Eq)

-- Your code goes here.
cons2list :: Cons a -> [a]
cons2list Nil = []
cons2list (Cons x (xs)) = x:cons2list xs
```

Passed!

Problem: `fib`
--------------

### Submitted Code

``` {.haskell}
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs _ [] = []
addpairs [] _ = []
addpairs (x:xs) (y:ys) = (x + y) : (addpairs xs ys)

-- Your code goes here.
fib :: [Integer]
fib = 1:1:addpairs fib xs
    where (_:xs) = fib
    
--fib = 1:1:addpairs fib (tail fib)
```

Passed!

Problem: `sumlist`
------------------

### Submitted Code

``` {.haskell}
-- Your code goes here.
--sumlist' :: (Num a) => [a] -> a
sumlist' :: (Num t) => [t] -> t
sumlist' xs = foldl1 (+) xs
```

### (-2) Functions `foldr1`/`foldl1` can't handle empty list

If you use the functinos `foldr1 :: (a -> b -> b) -> [a] -> b` or
`foldl1 :: (b -> a -> b) -> [a] -> b` you cannot handle the empty list anymore.

``` {.haskell}
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f (a:as) = f a (foldr f as)
```

Notice how the base-case `foldr1 f []` is not handled. Thus your function will
fail on the base-case `sumlist' []`, when it should return `0`.

Use `foldr :: (a -> b -> b) -> b -> [a] -> b` instead (which you can provide
with a base-case as the second argument):

``` {.haskell}
sumlist' :: Num a => [a] -> a
sumlist' as = foldr (+) 0 as
```

Problem: `powerset`
-------------------

### Submitted Code

``` {.haskell}
import qualified Data.List as List

add :: (Ord a) => a -> [a] -> [a]
add n xs = List.sort (List.nub (List.insert n xs))

union :: (Ord a) => [a] -> [a] -> [a]
union xs ys = List.sort (List.nub (List.union xs ys))

intersect :: (Ord a) => [a] -> [a] -> [a]
intersect xs ys = List.sort (List.nub (List.intersect xs ys))

-- Your code goes here.
powerset :: (Ord a) => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (powerset xs) (map (add x) (powerset xs))
```

Passed!

Problem: `sumlist`
------------------

### Submitted Code

``` {.haskell}
-- Your code goes here.
--sumlist :: (Num a) => [a] -> a
sumlist :: (Num t) => [t] -> t
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs
```

Passed!