module Chapter22
  () where

import           Control.Monad                  ( join )
import           Data.Char
import           Text.ParserCombinators.ReadP   ( string )

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . reverse

fmapped :: [Char] -> [Char]
fmapped = cap <$> reverse

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  capped <- cap
  reved  <- rev
  return (capped, reved)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (,) <$> rev

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName    :: DogName
  , dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- Exercise: Reading Comprehension
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f <$> ra

instance Applicative (Reader r) where
  pure a = Reader (const a)
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> (rab r <$> ra) r

-- The Monad of Functions
foo :: (Functor f, Num b) => f b -> f b
foo r = fmap (+ 1) r

bar :: Foldable t => a1 -> t a2 -> (a1, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+ 1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Functor t, Num a, Foldable t) => t a -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: (Foldable t, Functor t, Num a2) => t a2 -> (t a2, Int)
frooty r = bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  Dog name <$> address

getDogRM' :: Person -> Dog
getDogRM' =
  runReader $ Reader dogName >>= (\name -> Dog name <$> Reader address)

-- Exercise: Reader Monad
-- 1
instance Monad (Reader r) where
  (>>=) (Reader ra) faRb = Reader $ \r -> runReader ((faRb . ra) r) r


-- Chapter Exercises
