-- p2.hs  type classes  cs421

-- Announcements:
--  Practice exam is up.
--  Still ~50 people not signed up for the real exam!
--  HW 1 is in your repository
--  MP 1 soon$^{TM}$

data Shell a = Bash a

instance Show a => Show (Shell a) where
  show (Bash a) = "Bash " ++ (show a)

instance Eq a => Eq (Shell a) where
  Bash x == Bash y  = x == y

instance Functor Shell where
  fmap f (Bash x) = Bash (f x)

instance Applicative Shell where
  pure x = Bash x
  Bash f <*> Bash x = Bash (f x)

finc x = fmap (+1) x

data Couldbe a = Sure a
               | Nope

instance Show a => Show (Couldbe a) where
  show (Sure x) = "Sure " ++ (show x)
  show Nope = "Nope"

instance Eq a => Eq (Couldbe a) where
  Sure x == Sure y  = x == y
  Nope == Nope  = True
  _ == _  = False

instance Functor Couldbe where
  fmap f (Sure x) = Sure (f x)
  fmap f Nope = Nope

instance Applicative Couldbe where
  pure x = Sure x
  Sure f <*> Sure x = Sure (f x)
  _ <*> _ = Nope

aplus a b = (+) <$> (pure a) <*> (pure b)

data Weird a = Foo a
             | Bar a a
             | Baz


instance Show a => Show (Weird a) where
  show (Foo x) = "Foo " ++ (show x)
  show (Bar x y) = "Bar " ++ (show x) ++ " " ++ (show y)
  show Baz = "Baz"

instance Eq a => Eq (Weird a) where
  Foo x == Foo y  = x == y
  Bar a b == Bar c d  = a == c && b == d
  Baz == Baz  = True
  _ == _  = False

instance Functor Weird where
  fmap f (Foo x) = Foo (f x)
  fmap f (Bar x y) = Bar (f x) (f y)
  fmap f Baz = Baz

instance Applicative Weird where
  pure x = Foo x
  Foo f <*> Foo x = Foo (f x)
  Foo f <*> Bar x y = Bar (f x) (f y)
  Bar f g <*> Bar x y = Bar (f x) (g y)
  Bar f g <*> Foo x = Bar (f x) (g x)
  _ <*> _ = Baz
