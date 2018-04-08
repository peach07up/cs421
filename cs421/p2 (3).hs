-- p2.hs cs421 monads!
-- 40 still not registered for the exam!!

-- minc m = m >>= (\x -> return $ x + 1)
-- minc (msqrt [10])

minc x = return $ x + 1
madd a b = return $ a + b

inc x = x >>= (\a -> return $ a + 1)

add x y = do
  a <- x
  b <- y
  return $ a + b

-- alternative notation

add' x y = x >>= (\a ->
           y >>= (\b -> return $ a + b))

-- instance Monad Maybe where
--   (Just x) >>= f = f x
--   Nothing  >>= f = Nothing
--
-- instance Monad [] where
--   xx >>= f = concatMap f xx
--   [2,4,5] => [3,5,6]

t1 = Just 10
t2 = Nothing
t3 = Just 20

t4 = []
t5 = [2]
t6 = [5,3,8]
t7 = [9,3]

-- What are the outputs to these?

p1 = add t1 t3
p2 = add t1 t2
p3 = inc t4
p4 = inc t5
p5 = inc t6
p6 = add t4 t5
p7 = add t5 t7
p8 = add t6 t7

data Either' a b = ELeft a
                 | ERight b
     deriving (Show, Eq)

instance Functor (Either' e) where
  fmap _ (ELeft x)  = ELeft x
  fmap f (ERight x) = ERight (f x)

instance Applicative (Either' e) where
  pure = ERight
  (ERight f) <*> (ERight x)  = ERight (f x)
  (ELeft x)  <*> _           = ELeft x
  _          <*> (ELeft x)   = ELeft x

instance Monad (Either' e) where
  return = pure
  (ERight x) >>= f  = f x
  (ELeft a) >>= _  = ELeft a

mrecip m = if (m /= 0) then return (1/m) else fail "/0 error"


data Counter a = Counter a Int
               deriving (Show,Eq)

instance Functor Counter where
  fmap f (Counter a i) = Counter (f a) i

instance Applicative Counter where
  pure x = Counter x 0
  (Counter f i) <*> (Counter x j) = Counter (f x) (i + j)

instance Monad Counter where
  return x = Counter x 0
  (Counter a i) >>= f = let (Counter b j) = f x
                         in ....
