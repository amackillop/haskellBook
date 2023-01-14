{-# LANGUAGE InstanceSigs #-}

module Morse
  ( Morse,
    charToMorse,
    morseToChar,
    stringToMorse,
    letterToMorse,
    morseToLetter,
  )
where

import Control.Monad (forever, when)
import qualified Data.Foldable as M
import Data.List (intercalate, sort)
import qualified Data.Map as M
import Data.Traversable (traverse)
import GHC.Generics (UInt)
import GHC.IO.FD (stdin)
import GHC.IO.Handle (isEOF)
import GHC.Natural (Natural)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Property (Prop)
import GHC.Base (VecElem(DoubleElemRep))

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse =
  M.fromList
    [ ('a', ".-"),
      ('b', "-..."),
      ('c', "-.-."),
      ('d', "-.."),
      ('e', "."),
      ('f', "..-."),
      ('g', "--."),
      ('h', "...."),
      ('i', ".."),
      ('j', ".---"),
      ('k', "-.-"),
      ('l', ".-.."),
      ('m', "--"),
      ('n', "-."),
      ('o', "---"),
      ('p', ".--."),
      ('q', "--.-"),
      ('r', ".-."),
      ('s', "..."),
      ('t', "-"),
      ('u', "..-"),
      ('v', "...-"),
      ('w', ".--"),
      ('x', "-..-"),
      ('y', "-.--"),
      ('z', "--.."),
      ('1', ".----"),
      ('2', "..---"),
      ('3', "...--"),
      ('4', "....-"),
      ('5', "....."),
      ('6', "-...."),
      ('7', "--..."),
      ('8', "---.."),
      ('9', "----."),
      ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  line <- getLine
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        Just str -> putStrLn (unwords str)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  line <- getLine
  convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        Just s -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
      "from" -> convertFromMorse
      "to" -> convertToMorse
      _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn "Please specify the first argument as being 'from' or 'to' morse. Such as: morse to"
      exitFailure

-- TESTS
allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  Identity <$> arbitrary

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

instance Arbitrary a => Arbitrary (Identity a) where arbitrary = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenCharPair :: Gen (Sum Char (Pair Char Int))
sumGenCharPair = sumGenEqual

half :: Fractional a => a -> a
half x = x / 2

-- intGen :: (Fractional a, Arbitrary a) => Gen a
-- intGen = arbitrary

-- prop_halfIdentity :: Property
-- prop_halfIdentity = forAll intGen (\a -> 2 * half a == a)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_sortOrdersAList :: [Int] -> Bool
prop_sortOrdersAList = listOrdered . sort

isCommutative :: Eq a => (t -> t -> a) -> t -> t -> Bool
isCommutative op x y = x `op` y == y `op` x

isAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

prop_plusIsAssociative :: Int -> Int -> Int -> Bool
prop_plusIsAssociative = isAssociative (+)

prop_plusIsCommutative :: Integer -> Integer -> Bool
prop_plusIsCommutative = isCommutative (+)

prop_multiplyIsAssociative :: Int -> Int -> Int -> Bool
prop_multiplyIsAssociative = isAssociative (*)

prop_multiplyIsCommutative :: Integer -> Integer -> Bool
prop_multiplyIsCommutative = isCommutative (*)

prop_powerIsAssociative :: Int -> Int -> Int -> Bool
prop_powerIsAssociative = isAssociative (^)

prop_powerIsCommutative :: Int -> Int -> Bool
prop_powerIsCommutative = isCommutative (^)

prop_quotProp1 :: Natural -> Natural -> Bool
prop_quotProp1 x y = quot x y * y + rem x y == x

instance Arbitrary Natural where arbitrary :: Gen Natural
                                 arbitrary = elements [1 :: Natural .. 100]

naturals :: Gen Natural
naturals = elements [1 ..]

intGreaterThanZero :: Gen Int
intGreaterThanZero = elements [1 .. 100]

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

square :: Floating a => a -> a
square x = x * x

test :: IO ()
test = do
  _ <- quickCheck prop_thereAndBackAgain
  _ <- quickCheck $ forAll (arbitrary :: Gen Double) (\a -> 2 * half a == a)
  _ <- quickCheck prop_sortOrdersAList
  _ <- quickCheck prop_plusIsAssociative
  _ <- quickCheck prop_plusIsCommutative
  _ <- quickCheck prop_multiplyIsAssociative
  _ <- quickCheck prop_multiplyIsCommutative
  _ <- quickCheck $ forAll
    (genTuple :: Gen (Integer, NonZero Integer))
    (\(x, y) -> let y' = getNonZero y in quot x y' * y' + rem x y' == x)
  _ <- quickCheck $ forAll
    (genTuple :: Gen (Integer, NonZero Integer))
    (\(x, y) -> let y' = getNonZero y in div x y' * y' + mod x y' == x)
  -- _ <- quickCheck prop_powerIsAssociative
  -- _ <- quickCheck prop_powerIsCommutative
  _ <- quickCheck $ forAll (arbitrary :: Gen [Int]) (\a -> reverse (reverse a) == id a)
  -- _ <- quickCheck $ forAll (genTuple :: Gen ([Int], [Int])) (\(as, bs) -> foldr (:) as bs == as ++ bs)
  _ <- quickCheck $ forAll (arbitrary :: Gen [[Int]]) (\as -> foldr (++) [] as == concat as)
  -- _ <- quickCheck $ forAll (genTuple :: Gen (Int, [Int])) (\(n, as) -> length (take n as) == n)
  _ <- quickCheck $ forAll (arbitrary :: Gen Int) (\a -> read (show a) == a)
  -- _ <- quickCheck $ forAll (arbitrary :: Gen (Positive Double)) (\a -> let a' = getPositive a in square (sqrt a') == a')
  return ()