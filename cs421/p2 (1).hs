-- p2.hs
--
-- Announcements: mp2 released!
-- This is your "activity" for last two lectures...

-- (env) e1 || v1    (env) e2 || v2
-- ---------------------------------
--     (env) e1 + e2 || v1 + v2
--
--
-- (env) e1 || True    (env) e2 || v2
-- --------------------------------
-- (env) if e1 then e2 else e3 || v2

import Debug.Trace

-- problem 1
sumList [] k = k 0
sumList (x:xs) k = traceStack "here" $ sumList xs (\v -> k $ x + v)

-- problem 2
kmap f [] k = k []
kmap f (x:xs) k = kmap f xs (\vxs -> k $ f x : vxs)

-- pre problem 3
-- kfmap f [] k =  []
-- kfmap f (x:xs) k = f x (\vx -> vx  : kfmap f xs)

problem 3 v1
kfmap1 f [] k = k []
kfmap1 f (x:xs) k = f x (\vx ->
                    kfmap1 f xs (\vxs ->
                    k (vx : vxs)))

-- problem 3 v2
kfmap2 f [] k = k []
kfmap2 f (x:xs) k = kfmap2 f xs (\vxs ->
                    f x (\vx -> k (vx : vxs)))

timesk 0 _ ks kf = kf 0
timesk _ 0 ks kf = kf 0
timesk a b ks kf = ks $ trace "*" (a * b)








-- kmap inc [1,2,3] id
-- kmap inc [2,3] (\v -> id $ inc 1 : v)
-- kmap inc [3] (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v'))
-- kmap inc [] (\v'' -> (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v'))
--                      (3 : v''))
-- (\v'' -> (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v')) (inc 3 : v'')) []
--                      (3 : v''))
-- (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v')) (inc 3 :[]))
-- (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v')) [4]))
-- (\v -> id $ inc 1 : v) (inc 2 : [4])
-- (\v -> id $ inc 1 : v) [3,4])
-- id $ inc 1 : [3,4]
-- id $ [2,3,4]
-- [2,3,4]

data Calc = Add Integer
          | Sub Integer
     deriving (Show,Eq)

calc ((Add i):xs) ka ks = calc xs (\v -> ka $ trace (show v) $ i + v) ks
calc ((Sub i):xs) ka ks = calc xs ka (\v -> ks $ trace (show v) $ v - i)
calc []           ka ks = ks $ (ka 0)
