module Chapter18
  (List(..)) where

import           Control.Monad                  ( (>=>)
                                                , ap
                                                , forM
                                                , join
                                                , liftM2
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                )
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , Property
                                                , frequency
                                                , oneof
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative
                                                , monad
                                                )

import           Chapter17                      ( Identity(..)
                                                , List(..)
                                                )
import           Data.Monoid                    ( Sum(..) )
import Test.QuickCheck.Property

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join (fmap f m)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x then [x * x, x * x] else []

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "


-- Chapter Exercises
--1. 
data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

--2. 
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = frequency [(3, PLeft <$> arbitrary), (1, PRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Functor (BahEither b) where
  fmap fab (PLeft  a) = PLeft (fab a)
  fmap _   (PRight b) = PRight b

instance Monoid b => Applicative (BahEither b) where
  pure = PLeft
  (PLeft  fab) <*> (PLeft  a) = PLeft (fab a)
  (PLeft  fab) <*> (PRight b) = PRight b
  (PRight b  ) <*> _          = PRight b

instance Monoid b => Monad (BahEither b) where
  (PLeft  a) >>= f = f a
  (PRight b) >>= _ = PRight b

--3.
instance Monad Identity where
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4.
instance Monad List where
  (>>=) xs f = join $ f <$> xs

-- 5. 
j :: Monad m => m (m a) -> m a
j = join

-- 6. 
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 7. 
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 8. 
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 9.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = forM

-- 10.
flipType :: (Monad m) => [m a] -> m [a]
flipType = sequence

test :: IO ()
test = hspec $ do
  describe "Chapter18 Test Suite" $ do
    it "Nope obeys the monad laws" $ do
      quickBatch (monad (undefined :: Nope (Sum Int, Sum Int, Int)))
    it "BahEither obeys the monad laws" $ do
      quickBatch
        (monad (undefined :: BahEither (Sum Int) (Sum Int, Sum Int, Int)))
    it "Identity obeys the monad laws" $ do
      property $ quickBatch (monad (undefined :: Identity (String, String, Int)))
    it "List obeys the monad laws" $ do
      quickBatch (monad (undefined :: List (Sum Int, Sum Int, Sum Int)))

