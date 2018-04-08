-- p1.hs cs421 monads!
-- 40 still not registered for the exam!!

-- minc m = m >>= (\x -> return $ x + 1)
-- minc (msqrt [10])

inc x = x >>= (\a -> return $ a + 1)
inc1 x = return $ x + 1
add' x y = x >>= (\a ->
           y >>= (\b -> return $ a + b))

add x y = do
     a <- x
     b <- y
     return $ a + b

-- instance Monad [] where
--   return x = [x]
--   (>>=) xx f = concatMap f xx


-- instance Monad Maybe where
--   return x = Just x
--   (>>=) Nothing f = Nothing
--   (>>=) (Just a) f = f a

-- data Either a b = Left a | Right b
--
-- instance Applicative (Either e) where
--   pure = Right
--   (Right f) <*> (Right x) = Right (f x)
--   (Left e) <*> _          = Left e
--   _ <*> (Left e)          = Left e
--
-- instance Monad (Either e) where
--   return x = pure x
--   (Left x) >>= f = Left x
--   (Right x) >>= f = f x

t1 = Just 10
t2 = Nothing
t3 = Just 20
t4 = []
t5 = [2]
t6 = [5,3,8]
t7 = [9,3]

data Weird a = Bar a a
  deriving Show

instance Functor Weird where
  fmap f (Bar x y) = Bar (f x) (f y)

instance Applicative Weird where
  pure a = Bar a a
  (Bar f g) <*> (Bar x y) = Bar (f x) (g y)

instance Monad Weird where
  return = pure
  (Bar x y) >>= f = let (Bar a b) = f x
                        (Bar c d) = f y
                     in (Bar a d)
