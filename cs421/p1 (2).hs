-- p1.hs  type classes  cs421

-- Announcements:
--  Practice exam is up.
--  Still ~60 people not signed up for the real exam!

data Foo a = Bash a

instance Show a => Show (Foo a) where
  show (Bash a) = "Bash " ++ (show a)

instance Eq a => Eq (Foo a) where
  (Bash x) == (Bash y)  = x == y

instance Functor Foo where
  fmap f (Bash x) = Bash (f x)

instance Applicative Foo where
  pure x = Bash x
  (Bash f) <*> (Bash x) = Bash (f x)

finc x = fmap (+1) x

data Couldbe a = Sure a
               | Nope

instance Show a => Show (Couldbe a) where
  show (Sure a) = "Sure " ++ (show a)
  show Nope = "Nope"

instance Eq a => Eq (Couldbe a) where
  (Sure x) == (Sure y)  = x == y
  Nope == Nope = True
  _ == _ = False

instance Functor Couldbe where
  fmap f (Sure x) = Sure (f x)
  fmap f Nope = Nope

instance Applicative Couldbe where
  pure x = Sure x
  (Sure f) <*> (Sure x) = Sure (f x)
  _ <*> _ = Nope

data Weird a = Foo a
             | Bar a a
             | Baz

instance Show a => Show (Weird a) where
   show (Foo x) = "Foo " ++ (show x)
   show (Bar x y) = "Bar " ++ show x ++ " " ++ show y
   show Baz = "Baz"

instance Eq a => Eq (Weird a) where
  (Foo x) == (Foo y) = x == y
  (Bar a b) == (Bar c d) = a == c && b == d
  Baz == Baz = True
  _ == _ = False

instance Functor Weird where
  fmap f (Foo x) = Foo (f x)
  fmap f (Bar x y) = Bar (f x) (f y)
  fmap f Baz = Baz

instance Applicative Weird where
  pure a = Foo x
  Foo f <*> Foo x = Foo (f x)
  Foo f <*> (Bar a b) = Bar (f a) (f b)
  (Bar f g) <*> (Bar a b) = Bar (f a) (g b)
  (Bar f g) <*> (Foo a) = Bar (f a) (g a)
  _ <*> _ = Baz
