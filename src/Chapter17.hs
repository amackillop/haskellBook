module Chapter17
  ( Constant(..)
  , Two(..)
  , Three(..)
  , Three'(..)
  , Four'(..)
  , Identity(..)
  , List(..)
  ) where

import           Data.List                      ( elemIndex )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , frequency
                                                , oneof
                                                )
import           Test.QuickCheck.Checkers       ( EqProp(..)
                                                , eq
                                                , quickBatch
                                                )
import           Test.QuickCheck.Classes        ( applicative )

import           Chapter16                      ( Four(..)
                                                , Four'(..)
                                                , Identity(..)
                                                , List(..)
                                                , Pair(..)
                                                , Three(..)
                                                , Three'(..)
                                                , Two(..)
                                                )
import           Control.Applicative            ( liftA3 )
import           Data.Foldable                  ( Foldable(toList) )
import           Data.Monoid                    ( Sum(Sum) )
import           GHC.Base                       ( Nat )

-- Exercises: Lookups
-- 1.
added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1 .. 3] [4 .. 5])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1 .. 3] [4 .. 6]

z :: Maybe Integer
z = lookup 2 $ zip [1 .. 3] [4 .. 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--3.
j :: Maybe Int
j = elemIndex 3 [1 .. 5]

k :: Maybe Int
k = elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> j <*> k

-- 4.
xs = [1, 2, 3]
ys = [4, 5, 6]

n :: Maybe Integer
n = lookup 3 $ zip xs ys

m :: Maybe Integer
m = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
-- summed = (sum $) <*> ((,) n m)

-- Exercise: Identity Instance
-- newtype Identity a = Identity a deriving (Eq, Ord, Show)

-- instance Functor Identity where
--   fmap fab (Identity a) = Identity (fab a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity fab) (Identity a) = Identity (fab a)

-- Exercise: Constant Instance
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a <> b)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- Exercise: List Applicative
instance Semigroup (List a) where
  (<>) Nil          Nil           = Nil
  (<>) c@(Cons _ _) Nil           = c
  (<>) Nil          c@( Cons _ _) = c
  (<>) (Cons a as)  bs@(Cons _ _) = Cons a (as <> bs)

instance Monoid (List a) where
  mempty = Nil

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _   = Nil
  (<*>) _   Nil = Nil
  (<*>) (Cons fab rest) (Cons a as) =
    Cons (fab a) (fab <$> as) <> (rest <*> Cons a as)

instance Foldable List where
  foldr _ b   Nil        = b
  foldr f acc (Cons h t) = f h (foldr f acc t)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) xs ys = eq (take 100 $ toList xs) (take 100 $ toList ys)

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = foldr (<>) mempty (f <$> as)


-- Exercise: ZipList Applicative
newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' = let (ZipList' l) = xs in take 100 l
    ys' = let (ZipList' l) = ys in take 100 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (repeat a)
  (<*>) (ZipList' fabs) (ZipList' as) = ZipList' (zipWith ($) fabs as)

-- Exercise: Variations on Either
data Validation e a = Failure e | Success a deriving (Eq, Show)


instance Functor (Validation e) where
  fmap _   (Failure e) = Failure e
  fmap fab (Success a) = Success (fab a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure e1 ) (Failure e2) = Failure (e1 <> e2)
  (<*>) (Failure e1 ) (Success _ ) = Failure e1
  (<*>) (Success _  ) (Failure e1) = Failure e1
  (<*>) (Success fab) (Success a ) = Success (fab a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [(1, Failure <$> arbitrary), (3, Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

-- Chapter Exercises
-- 1. 
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = (Pair <$> arbitrary) <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2.
instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = (Two <$> arbitrary) <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--3.
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.
instance Monoid a => Applicative (Three' a) where
  pure f = Three' mempty f f
  (<*>) (Three' a f1 f2) (Three' a' b1 b2) = Three' (a <> a') (f1 b1) (f2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' d) =
    Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.
instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' b) =
    Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

test :: IO ()
test = hspec $ do
  describe "Chapter17 Test Suite" $ do
    it "Constant obeys the applicative laws" $ do
      quickBatch
        (applicative (undefined :: Constant String (String, String, Int)))
    it "List obeys the applicative laws" $ do
      quickBatch (applicative (undefined :: List (String, String, Int)))
    it "ZipList' obeys the applicative laws" $ do
      quickBatch (applicative (undefined :: ZipList' (String, String, Int)))
    it "Validation obeys the applicative laws" $ do
      quickBatch
        (applicative (undefined :: Validation String (String, String, Int)))
    it "Pair obeys the applicative laws" $ do
      quickBatch (applicative (undefined :: Pair (String, String, Int)))
    it "Two obeys the applicative laws" $ do
      quickBatch
        (applicative
          (undefined :: Two (String, String, Sum Int) (String, String, Sum Int))
        )
    it "Three obeys the applicative laws" $ do
      quickBatch
        (applicative
          (undefined :: Three
              (String, String, Sum Int)
              (String, String, Sum Int)
              (String, String, Sum Int)
          )
        )
    it "Three' obeys the applicative laws" $ do
      quickBatch
        (applicative
          (undefined :: Three'
              (String, String, Sum Int)
              (String, String, Sum Int)
          )
        )
    it "Four obeys the applicative laws" $ do
      quickBatch
        (applicative
          (undefined :: Four
              (String, String, Sum Int)
              (String, String, Sum Int)
              (String, String, Sum Int)
              (String, String, Sum Int)
          )
        )
    it "Four' obeys the applicative laws" $ do
      quickBatch
        (applicative
          (undefined :: Four'
              (String, String, Sum Int)
              (String, String, Sum Int)
          )
        )
    it "combos correctly combines lists" $ do
      combos "ab" "cd" "ef"
        == [ ('a', 'c', 'e')
           , ('a', 'c', 'f')
           , ('a', 'd', 'e')
           , ('a', 'd', 'f')
           , ('b', 'c', 'e')
           , ('b', 'c', 'f')
           , ('b', 'd', 'e')
           , ('b', 'd', 'f')
           ]

