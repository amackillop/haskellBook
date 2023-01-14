{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Chapter16
  ( List(..)
  , Identity(..)
  , Pair(..)
  , Two(..)
  , Three(..)
  , Three'(..)
  , Four(..)
  , Four'(..)
  ) where

import           GHC.Arr                        ( Array )
import           Test.QuickCheck                ( Large(getLarge)
                                                , quickCheck
                                                )

-- Exercises: Heavy Listing
-- 1.
a :: [Int]
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2.
b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c :: Integer -> Integer
c = fmap (* 2) (\x -> x - 2)

-- 4.
d :: Int -> [Char]
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let lol     = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) lol
  in  fmap (* 3) changed

-- Exercises: Instances of Func
-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

-- 6.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

-- 8.
-- Can't do it. Wrong kind
data Trivial = Trivial

-- Chapter Exercises
-- 1. No Functor for Bool

-- 2.
data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap fab (False' a) = False' (fab a)
  fmap fab (True'  a) = True' (fab a)

-- 3.
data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _   Falsish    = Falsish
  fmap fab (Truish a) = Truish (fab a)

-- 4. No Functor. Kind of Mu is (* -> *) -> *, needs to be * -> *
newtype Mu f = Inf { outF :: f (Mu f) }

-- 5. No Functor. Kind of D is *, needs to be * -> *
data D = D (Array Word Word) Int Int

-- 1.
data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First  a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b ) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


-- 3.
data More b a = L a b a | R b a b deriving (Eq, Show)

-- 1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _   Finance   = Finance
  fmap _   (Desk  a) = Desk a
  fmap fab (Bloor a) = Bloor (fab a)

-- 2.
-- data K a b = K a

instance Functor (K a) where
  fmap f k@(K a) = K a

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap fab (Flip k) = Flip
    (case k of
      (K a) -> K (fab a)
    )

-- 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap fab (GoatyConst a) = GoatyConst (fab a)

-- 5.
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap fab (LiftItOut f) = LiftItOut (fab <$> f)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap fab (DaWrappa fa ga) = DaWrappa (fmap fab fa) (fmap fab ga)

--7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap fab (IgnoringSomething fa fb) = IgnoringSomething fa (fab <$> fb)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap fab (Notorious go ga gt) = Notorious go ga (fmap fab gt)

-- 9.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _   Nil         = Nil
  fmap fab (Cons a as) = Cons (fab a) (fmap fab as)

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _   NoGoat      = NoGoat
  fmap fab (OneGoat a) = OneGoat (fab a)
  fmap fab (MoreGoats ga ga' ga'') =
    MoreGoats (fmap fab ga) (fmap fab ga) (fmap fab ga)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _   Halt        = Halt
  fmap fab (Print s a) = Print s (fab a)
  fmap fab (Read sa  ) = Read (fab . sa)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

test :: IO ()
test = do
  print $ a == [2]
  print $ b == Just ["Hi,lol", "Hellolol"]
  print $ c 1 == -2
  print $ d (0 :: Int) == "1[0,1,2,3]"
  e' <- e
  print $ e' == 3693
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose (+ 1) (* 2) :: [Int] -> Bool)


data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)




type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]

degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]

type BadNat f g a = f a -> g a

degenerateMtl' :: Num a => BadNat Maybe [] a
degenerateMtl' Nothing  = []
degenerateMtl' (Just a) = [a + 1]

getInt :: IO Int
getInt = read <$> getLine

main :: IO ()
main = undefined
