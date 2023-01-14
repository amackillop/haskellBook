{-# LANGUAGE InstanceSigs #-}
module Chapter21
  () where
import           Test.Hspec
import           Test.Hspec.Checkers            ( testBatch )
import           Test.QuickCheck
import           Test.QuickCheck.Classes        ( traversable )

import           Chapter16                      ( Three'(..) )
import           Chapter18                      ( List(..) )
import           Chapter20                      ( Three(..)
                                                , Two(..)
                                                )
import           Data.Monoid                    ( Sum )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck.Checkers

-- Chapter Exercises
-- 1
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
-- 2
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

-- 3
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _   Nada    = Nada
  fmap fab (Yep a) = Yep (fab a)

instance Foldable Optional where
  foldMap f Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse f Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(3, Yep <$> arbitrary), (1, return Nada)]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

-- 4
instance Traversable List where
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t
  traverse f Nil        = pure Nil

-- 5
instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- 6
instance Traversable (Two a) where
  traverse f (Two a b) = Two a <$> f b

-- 7
instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2


-- 9
data S n a = S (n a) a
  deriving (Eq, Show)

instance ( Functor n , Arbitrary (n a) , Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n , Testable (n Property) , Eq a , Eq (n a) , EqProp a) => EqProp (S n a) where
  (=-=) = eq

instance (Functor n) => Functor (S n) where
  fmap f (S n a) = S (f <$> n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

main = sample' (arbitrary :: Gen (S [] Int))

-- 10
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t1 a t2) = Node (f <$> t1) (f a) (f <$> t2)

instance Foldable Tree where
  foldMap f Empty          = mempty
  foldMap f (Leaf a      ) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

instance Traversable Tree where
  traverse f Empty          = pure Empty
  traverse f (Leaf a      ) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof
    [ pure Empty
    , Leaf <$> arbitrary
    , Node <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

test = hspec $ do
  describe "Chapter21 Test Suite" $ do
    prop "Identity obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Identity (Maybe Int, Maybe Int, Maybe Int, Sum Int))
        )
    prop "Constant obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Constant
              (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
              (Maybe Int, Maybe Int, Maybe Int, Sum Int)
          )
        )
    prop "Maybe obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Maybe (Maybe Int, Maybe Int, Maybe Int, Sum Int))
        )
    prop "List obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: List (Maybe Int, Maybe Int, Maybe Int, Sum Int))
        )
    prop "Three obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Three
              (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
              (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
              (Maybe Int, Maybe Int, Maybe Int, Sum Int)
          )
        )
    prop "Two obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Two
              (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
              (Maybe Int, Maybe Int, Maybe Int, Sum Int)
          )
        )
    prop "Three' obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Three'
              (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
              (Maybe Int, Maybe Int, Maybe Int, Sum Int)
          )
        )
    prop "S obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: S Maybe (Maybe Int, Maybe Int, Maybe Int, Sum Int))
        )
    prop "Tree obeys the traversable laws" $ do
      property $ quickBatch
        (traversable
          (undefined :: Tree (Maybe Int, Maybe Int, Maybe Int, Sum Int))
        )
