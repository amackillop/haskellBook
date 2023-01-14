{-# LANGUAGE InstanceSigs #-}

module Chapter15 () where

import Control.Monad
import Data.Monoid
import Data.Semigroup
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) :: Semigroup a => Optional a -> Optional a -> Optional a
  (<>) (Only x) (Only y) = Only (x <> y)
  (<>) (Only x) Nada = Only x
  (<>) Nada (Only y) = Only y
  (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty :: Optional a
  mempty = Nada

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary :: Gen Bull
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) :: Bull -> Bull -> Bull
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty :: Bull
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) :: First' a -> First' a -> First' a
  (<>) a@(First' (Only _)) _ = a
  (<>) _ b@(First' (Only _)) = b
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty :: First' a
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary :: Arbitrary a => Gen (First' a)
  arbitrary = do
    a <- arbitrary
    frequency [(3, return $ First' (Only a)), (1, return $ First' Nada)]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

test :: IO ()
test = hspec $ do
  describe "Chapter15" $ do
    it "mappend takes first success one" $ do
      First' (Only 1) <> First' Nada == First' (Only (1 :: Integer))
    it "mappend takes first success two" $ do
      First' (Nada) <> First' (Only 2) == First' (Only (2 :: Integer))
    it "mappend takes first success three" $ do
      First' (Only 1) <> First' (Only 2) == First' (Only (1 :: Integer))
    it "mappend takes nada if both failed" $ do
      First' Nada <> First' Nada == First' (Nada :: Optional String)
    it "mappend is associative" $ do
      quickCheck (monoidAssoc :: FirstMappend)
    it "mappend obeys left identity" $ do
      quickCheck (monoidLeftIdentity :: FstId)
    it "mappend obeys right identity" $ do
      quickCheck (monoidRightIdentity :: FstId)

-- Exercises
-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) :: Trivial -> Trivial -> Trivial
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty :: Trivial
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary :: Gen Trivial
  arbitrary = return Trivial

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) :: Semigroup a => Identity a -> Identity a -> Identity a
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty :: Identity a
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary :: Arbitrary a => Gen (Identity a)
  arbitrary = Identity <$> arbitrary

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) :: (Semigroup a, Semigroup b) => Two a b -> Two a b -> Two a b
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty :: Two a b
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) :: Three a b c -> Three a b c -> Three a b c
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary :: (Arbitrary a, Arbitrary b) => Gen (Three a b c)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) :: Four a b c d -> Four a b c d -> Four a b c d
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary :: Gen (Four a b c d)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) :: BoolConj -> BoolConj -> BoolConj
  (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
  mempty :: BoolConj
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary :: Gen BoolConj
  arbitrary = BoolConj <$> arbitrary

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) :: BoolDisj -> BoolDisj -> BoolDisj
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Monoid BoolDisj where
  mempty :: BoolDisj
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary :: Gen BoolDisj
  arbitrary = BoolDisj <$> arbitrary

-- 8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) :: Or a b -> Or a b -> Or a b
  (<>) (Snd a) _ = Snd a
  (<>) _ b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary :: Gen (Or a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

-- instance Monoid Trivial where
--   mempty :: Trivial
--   mempty = Trivial

--   mappend :: Trivial -> Trivial -> Trivial
--   mappend = (<>)

-- 9.
newtype Combine a b = Combine {unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) :: Combine a b -> Combine a b -> Combine a b
  (<>) (Combine f) (Combine g) = Combine (\a -> f a <> g a)

instance Monoid b => Monoid (Combine a b) where
  mempty :: Combine a b
  mempty = Combine (const mempty)

instance (Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary :: Gen (Combine a b)
  arbitrary = Combine <$> arbitrary

-- 10.
newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup a => Semigroup (Comp a) where
  (<>) :: Comp a -> Comp a -> Comp a
  (<>) (Comp f) (Comp g) = Comp (f <> g)

instance Monoid a => Monoid (Comp a) where
  mempty :: Monoid a => Comp a
  mempty = Comp mempty

-- 11.
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) :: Validation a b -> Validation a b -> Validation a b
  (<>) s@(Success' _) _ = s
  (<>) _ s@(Success' _) = s
  (<>) (Failure' a) (Failure' b) = Failure' (a <> b)

instance (Arbitrary a, Arbitrary b, Semigroup b) => Arbitrary (Validation a b) where
  arbitrary :: Gen (Validation a b)
  arbitrary = oneof [Success' <$> arbitrary, Failure' <$> arbitrary]


-- Monoid 8.
newtype Mem s a = Mem {
  runMem :: s -> (a, s)
}

instance Semigroup a => Semigroup (Mem s a) where
  (<>) :: Semigroup a => Mem s a -> Mem s a -> Mem s a
  (<>) (Mem f) (Mem g) = Mem (\s -> let a = f s
                                        b = g s
                                    in (fst a <> fst b, snd (f $ snd (g s))))

instance Monoid a => Monoid (Mem s a) where
  mempty :: Monoid a => Mem s a
  mempty = Mem (\s -> (mempty, s))

f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

g' :: Mem Integer String
g' = Mem $ \s -> ("hi", s + 3)

h' :: Mem Integer String
h' = Mem $ \s -> ("hi", s + 3)

main' :: IO ()
main' = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
      rmmappend1 = runMem (f' <> (g' <> h')) 0
      rmmappend2 = runMem ((f' <> g') <> h') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
  print $ rmmappend1 == rmmappend2
  print rmmappend1

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = Or Int String -> Or Int String -> Or Int String -> Bool

type CombineAssocF = Int -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Bool

type CompAssocF = Sum Int -> Fun (Sum Int) (Sum Int) -> Fun (Sum Int) (Sum Int) -> Fun (Sum Int) (Sum Int) -> Bool

type ValidationAssoc = Validation (Sum Int) String -> Validation (Sum Int) String -> Validation (Sum Int) String -> Bool

type MemAssocF = Int -> Fun Int (String, Int) -> Fun Int (String, Int) -> Fun Int (String, Int) -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupAssocF :: (Eq a1, Semigroup m) => ((a2 -> b) -> m) -> (m -> p -> a1) -> p -> Fun a2 b -> Fun a2 b -> Fun a2 b -> Bool
semigroupAssocF wrapper eval input f g h =
  let f' = (wrapper . apply) f
      g' = (wrapper . apply) g
      h' = (wrapper . apply) h
      left = eval (f' <> (g' <> h')) input
      right = eval ((f' <> g') <> h') input
   in left == right

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidLeftIdentityF :: (Eq a1, Monoid m) => ((a2 -> b) -> m) -> (m -> t2 -> a1) -> t2 -> Fun a2 b -> Bool
monoidLeftIdentityF wrapper eval input f =
  let f' = (wrapper . apply) f
   in eval (mempty <> f') input == eval f' input

monoidRightIdentityF :: (Eq a1, Monoid m) => ((a2 -> b) -> m) -> (m -> t2 -> a1) -> t2 -> Fun a2 b -> Bool
monoidRightIdentityF wrapper eval input f =
  let f' = (wrapper . apply) f
   in eval (f' <> mempty) input == eval f' input

runQc :: IO ()
runQc = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two String [Int] -> Bool)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck $ forAll (arbitrary :: Gen (Bool, Bool)) (\(a, b) -> BoolConj a <> BoolConj b == BoolConj (a && b))
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck $
    forAll
      (arbitrary :: Gen (Or Int String, Or Int String))
      ( \(a, b) -> case a of
          Snd _ -> a <> b == a
          _ -> a <> b == b
      )
  quickCheck (semigroupAssocF Combine unCombine :: CombineAssocF)
  quickCheck (monoidLeftIdentityF Combine unCombine :: Int -> Fun Int (Sum Int) -> Bool)
  quickCheck (monoidRightIdentityF Combine unCombine :: Int -> Fun Int (Sum Int) -> Bool)
  quickCheck (semigroupAssocF Comp unComp :: CompAssocF)
  quickCheck (monoidLeftIdentityF Comp unComp :: Sum Int -> Fun (Sum Int) (Sum Int) -> Bool)
  quickCheck (monoidRightIdentityF Comp unComp :: Sum Int -> Fun (Sum Int) (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssocF Mem runMem :: MemAssocF)
  quickCheck (monoidLeftIdentityF Mem runMem :: Int -> Fun Int (String, Int) -> Bool)
  quickCheck (monoidRightIdentityF Mem runMem :: Int -> Fun Int (String, Int) -> Bool)
  -- quickCheck (semigroupAssoc :: Min'Assoc)
  -- quickCheck (monoidLeftIdentity :: Min' Int -> Bool)
  -- quickCheck (monoidRightIdentity :: Min' Int -> Bool)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

-- newtype Min a = Tree a
-- instance Monoid a => Monoid (Min a) where
--   mempty :: Monoid a => Min a
--   mempty = _

tree :: Tree Int
tree = Node (Node (Leaf 1) 8 (Leaf 0)) 7 (Leaf 2)

newtype Min' a = Min' {getMin' :: a} deriving (Eq, Show)

instance Ord a => Semigroup (Min' a) where
  (<>) :: Min' a -> Min' a -> Min' a
  (<>) (Min' a) (Min' b) = if a < b then Min' a else Min' b

instance (Ord a, Bounded a) => Monoid (Min' a) where
  mempty :: Ord a => Min' a
  mempty = Min' maxBound

instance Arbitrary a => Arbitrary (Min' a) where
  arbitrary :: Gen (Min' a)
  arbitrary = Min' <$> arbitrary


newtype Max' a = Max' {getMax' :: a} deriving (Eq, Show)

instance Ord a => Semigroup (Max' a) where
  (<>) :: Max' a -> Max' a -> Max' a
  (<>) (Max' a) (Max' b) = if a > b then Max' a else Max' b

instance (Ord a, Bounded a) => Monoid (Max' a) where
  mempty :: Ord a => Max' a
  mempty = Max' minBound

instance Arbitrary a => Arbitrary (Max' a) where
  arbitrary :: Gen (Max' a)
  arbitrary = Max' <$> arbitrary

type Min'Assoc = Min' Int -> Min' Int -> Min' Int -> Bool
type Max'Assoc = Max' Int -> Max' Int -> Max' Int -> Bool

main :: IO ()
main = do
  let a = foldMap (Any . (== 1)) tree
  let b = foldMap (All . (< 5)) tree
  let c = foldMap (\a -> (Min' a, Max' a)) tree
  print a
  print b
  print c