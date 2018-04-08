-- 9:30 section cs 421
import Debug.Trace

-- Point free functions...

plus a b = a + b

  -- eta reduction
inc1 a = plus 1 a
  -- is equivalent to
inc2 :: Num a => a -> a
inc2 = plus 1

-- Mapping and Folding

negList xx = map (\a -> -a) xx

negList' xx = map (* (-1)) xx

-- foldr f z [] = []
-- foldr f z (x:xs) = f x (foldr f z xs)

sumSqr xx = foldr (\a b -> trace (show a ++ " " ++ show b)
                           a * a + b) 0 xx
--sumSqr' = foldr ((+) . (^2)) 0

nest xx = foldr (\a b ->
                  ("(" ++ a ++ " " ++ b ++ ")")) "z" xx

lnest xx = foldl (\a b ->
                  ("(" ++ a ++ " " ++ b ++ ")")) "z" xx
-- map f xx = foldr (\a b -> f a : b) [] xx

-- Other HOFs

myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

fix f x = if x == result
           then result
          else fix f result
  where result = f x

fixn f x 0 = f x
fixn f x n = if x == result
           then result
          else fixn f result (n - 1)
  where result = f x

curry f = \a b -> f (a,b)

flip f a b = f b a

flop f a b = f a b
