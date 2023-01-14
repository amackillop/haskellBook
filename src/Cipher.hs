module Cipher where

import Data.Char (ord, chr)
import Test.QuickCheck

shiftChar :: Int -> Char -> Char
shiftChar n = chr . wrap . (+ n) . ord
 where
  wrap = (+ 97) . (`mod` 26) . (\x -> x - 97)

caesar :: Int -> String -> String
caesar n = map (shiftChar n)

uncaesar :: Int -> String -> String
uncaesar n = map (shiftChar $ negate n)

myOr :: [Bool] -> Bool
myOr xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a xs = myAny (== a) xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish = foldr1 (++)


vigenere :: String -> String -> String
vigenere key text = map (\(a,b) -> a b) (zip ciphers text)
 where
  ciphers :: [Char -> Char]
  ciphers = map (shiftChar . ((-) 65 . ord)) (cycle key)
  wrap = (+ (97 :: Int)) . (`mod` 26) . (\x -> x - 97)


runQc :: IO ()
runQc = do
  _ <- quickCheck $ forAll (arbitrary :: Gen (Int, String)) (\(n, s) -> uncaesar n (caesar n s) == s)
  -- _ <- quickCheck $ forAll (arbitrary :: Gen (String, String)) (\(n, s) -> uncaesar n (caesar n s) == s)
  return ()
