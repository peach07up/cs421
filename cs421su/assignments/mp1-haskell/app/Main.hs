module Main where
	
mytake :: Int -> [a] -> [a]  
mytake n _  
    | n <= 0   = []  
mytake _ []     = []  
mytake n (x:xs) = x : mytake (n-1) xs 

mydrop :: Int -> [a] -> [a]
mydrop n xs | n <=0 = xs
mydrop _ [] = []
mydrop n (_:xs) = mydrop (n-1) xs
          
rev :: [a] -> [a]
rev list = rev' list []
    where rev' [] reversed = reversed
          rev' (x:xs) reversed = rev' xs (x:reversed)
          
app :: [a] -> [a] -> [a]
app [] xs = xs
app xs [] = xs
app (x:xs) ys = x:app xs ys

inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1):inclist xs

sumlist :: (Num a) => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

myzip :: [a] -> [b] -> [(a,b)] 
myzip list [] = []
myzip [] list = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys 

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs list1 list2 = sumzip (myzip list1 list2)
        where sumzip [] = []
              sumzip ((a,b):xs) = a+b:sumzip xs

--addpairs (x:xs) (y:ys) = (x+y):addpairs xs ys

ones :: [Integer]
ones = 1:ones

nats :: [Integer]
nats = 0:inclist nats

--fib :: [Integer]
--fib = 0:1:addpairs fib (tail fib)

fib :: [Integer]
fib = 0:1:addpairs fib xs
    where (_:xs) = fib

add :: (Ord a) => a -> [a] -> [a]
add n [] = [n]
add n (x:xs)
    | n > x = x:add n xs
    | n < x = n:x:xs
    | otherwise = x:xs
    
    
maxnum :: (Ord a) => a -> a -> a
maxnum x y
    | x > y = x
    | x < y = y
    | otherwise = x
    
union :: (Ord a) => [a] -> [a] -> [a]
union [] xs = xs
union xs [] = xs
union (x:xs) (y:ys)
    | x < y = x:union xs (y:ys)
    | x > y = y:union (x:xs) ys
    | otherwise = x:union xs ys
    
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
    | x < y = intersect xs (y:ys)
    | x > y = intersect (x:xs) ys
    | otherwise = x:intersect xs ys
    
powerset :: (Ord a) => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (powerset xs) (map (add x) (powerset xs))

inclist' :: (Num a) => [a] -> [a]
inclist' xs = map (+1) xs

sumlist' :: (Num a) => [a] -> a
sumlist' xs = foldl (+) 0 xs

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x (xs)) = x:cons2list xs

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
		 deriving (Show, Eq)
		 
eval :: Exp -> Integer
eval (IntExp x) = x
eval (PlusExp xs) = foldl (+) 0 (map eval xs)
eval (MultExp xs) = foldl (*) 1 (map eval xs)

list2cons' :: [a] -> List a
list2cons' [] = Nil
list2cons' xs = foldr Cons Nil xs

data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf
  deriving (Show)
  
  
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node n a b) = n + (sumTree a) + (sumTree b)

data SimpVal = IntVal Integer
               | BoolVal Bool 
               | StrVal String
               | ExnVal String
			   deriving (Show)
  
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp (+) (IntVal x) (IntVal y) = IntVal (x + y)
liftIntOp (*) (IntVal x) (IntVal y) = IntVal (x * y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"