module Chapter14 where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a, Ord a) => a -> a -> a
multiplyBy left right
  | right >= 0 = go 0 0
  | otherwise = go' 0 0
  where
    go sum' count
      | count == right = sum'
      | otherwise = go (sum' + left) (count + 1)
    go' sum' count
      | count == right = sum'
      | otherwise = go' (sum' - left) (count - 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

main :: IO ()
main = hspec $ do
  describe "Chapter14" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0 :: Int)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2 :: Int)
    it "15 multiplied by 3 should be 45" $ do
      multiplyBy 15 3 `shouldBe` (45 :: Int)
    it "15 multiplied by 0 should be 0" $ do
      multiplyBy 15 0 `shouldBe` (0 :: Int)
    it "15 multiplied by -2 should be -30" $ do
      multiplyBy 15 (-2) `shouldBe` (-30 :: Int)
    it "multiplyBy behaves the same as (*)" $ do
      property $ \(x, y) -> multiplyBy x y == (x * y :: Int)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
