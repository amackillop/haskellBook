module Chapter20
  (Three(..), Two(..)) where
import           Data.Monoid                    ( All(..)
                                                , Any(Any, getAny)
                                                , Product(Product, getProduct)
                                                , Sum(Sum)
                                                , getSum
                                                )

import           Chapter17                      ( Four'(..)
                                                , Three(..)
                                                , Three'(..)
                                                , Two(..)
                                                )
import           Control.Exception              ( catchJust )
import           Data.Foldable                  ( toList )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( Min(Min, getMin) )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                )
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , Property
                                                , frequency
                                                , oneof
                                                , quickCheck
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative
                                                , monad
                                                )
import           Test.QuickCheck.Gen            ( Gen )
import           Test.QuickCheck.Property       ( forAll )
-- Exercises: Library Functions
-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (a ==))

-- 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
 where
  f a m = Just $ case m of
    Nothing -> a
    Just m' -> min a m'

-- 5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
 where
  f a m = Just $ case m of
    Nothing -> a
    Just m' -> max a m'

-- 6
null' :: (Foldable t) => t a -> Bool
null' = getAll . foldr (\_ _ -> All False) (All True)

-- 7
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty

-- Chapter Exercises
-- 1
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2
instance Foldable (Two a) where
  foldMap f (Two a b) = f b

-- 3
instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- 4
instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 <> f b2

-- 5
instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = f b3

-- 6
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

test :: IO ()
test = hspec $ do
  describe "Chapter20 test suite" $ do
    it "sum' should match sum" $ do
      forAll (arbitrary :: Gen [Int]) (\xs -> sum' xs == sum xs)
    it "product' should match product" $ do
      forAll (arbitrary :: Gen [Int]) (\xs -> product' xs == product xs)
    it "elem' should match elem" $ do
      forAll (arbitrary :: Gen (Int, [Int]))
             (\(a, xs) -> elem' a xs == elem a xs)
    prop "minimum' should match minimum" $ do
      forAll
        (arbitrary :: Gen [Int])
        (\xs -> case minimum' xs of
          Just m  -> m == minimum xs
          Nothing -> null xs
        )
    prop "maximum' should match maximum" $ do
      forAll
        (arbitrary :: Gen [Int])
        (\xs -> case maximum' xs of
          Just m  -> m == maximum xs
          Nothing -> null xs
        )
    prop "null' should match null" $ do
      forAll (arbitrary :: Gen [Int]) (\xs -> null' xs == null xs)
    prop "length' should match length" $ do
      forAll (arbitrary :: Gen [Int]) (\xs -> length' xs == length xs)
    prop "toList' should match toList" $ do
      forAll (arbitrary :: Gen [Int]) (\xs -> toList' xs == toList xs)
