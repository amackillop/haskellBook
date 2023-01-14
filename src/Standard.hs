module Standard where

myOr :: [Bool] -> Bool
myOr xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = myOr $ map f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x z -> f x || z) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a xs = myAny (== a) xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = foldr (\x z -> (a == x) || z) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr1 (++) . map f

squish :: [[a]] -> [a]
squish = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp = foldr1 (\ x y -> if cmp x y == GT then x else y)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp = foldr1 (\ x y -> if cmp x y == LT then x else y)

average :: (Fractional a, Num a) => [a] -> a
average xs =
  let (total, count) = foldr (\x (t, c) -> (x + t, c + 1)) (0, 0) xs
  in  total / count

(/:.) :: Foldable t => b -> t a -> (b -> a -> b) -> b
(z /:. xs) f = foldl f z xs

(.:\) :: Foldable t => t a -> b -> (a -> b -> b) -> b
(xs .:\ z) f = foldr f z xs

fibonaccis :: [Integer]
fibonaccis = 1 : scanl (+) 1 fibonaccis

factorials :: [Integer]
factorials = scanl (*) 1 [2..]

-- fold :: Monoid m => (a -> m) -> [a] -> m
-- fold f [] = mempty
-- fold f (x : xs) = f x <> fold f xs

-- newtype Endo a = Endo (a -> a)

-- instance Monoid (Endo a) where
--   mappend (Endo a) (Endo b) = Endo (a . b)
--   mempty = Endo id

-- foldr :: (a -> (b -> b)) -> b -> [a] -> b
-- foldr f z xs = fold 



























