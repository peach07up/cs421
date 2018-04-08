-- p1.hs
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

sumList [] k = k 0
sumList (x:xs) k = sumList xs (\v -> k $ v + x)

kmap f [] k = k []
kmap f (x:xs) k = kmap f xs (\fxs -> k $ f x : fxs)

kfmap f [] =  []
kfmap f (x:xs) = f x (\v -> v : kfmap f xs)

kfkmap f []     k = k []
kfkmap f (x:xs) k = f x (\v -> kfkmap f xs
                               (\vxs -> k $ v:vxs))
kfkmap f []     k = k []
kfkmap f (x:xs) k = kfkmap f xs (\vxs ->
                                 f x (\v -> k $ v:vsx))

kfoldr f z [] ks kf = ks z
kfoldr f z (x:xs) ks kf =
  kfoldr f z xs (\vxs -> f x vxs ks kf) kf

timesk 0 _ ks kf = kf 0
timesk _ 0 ks kf = kf 0
timesk a b ks kf = ks $ trace "*" (a * b)

-- kmap inc [1,2,3] id
-- kmap inc [2,3] (\v -> id $ inc 1 : v)
-- kmap inc [3] (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v'))
-- kmap inc [] (\v'' -> (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v'))
--                      (3 : v''))
-- (\v'' -> (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v')) (inc 3 : v'')) 0
--                      (3 : v''))
-- (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v')) (inc 3 :[]))
-- (\v' -> (\v -> id $ inc 1 : v) (inc 2 : v')) [4]))
-- (\v -> id $ inc 1 : v) (inc 2 : [4])
-- (\v -> id $ inc 1 : v) [3,4])
-- id $ inc 1 : [3,4]
-- id $ [2,3,4]
-- [2,3,4]
