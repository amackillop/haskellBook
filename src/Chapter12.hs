module Chapter12 where

countVowels :: String -> Integer
countVowels = foldl countIfVowel 0
 where
  countIfVowel acc x | x `elem` "aeiou" = acc + 1
                     | otherwise        = acc

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str =
  let (a, b) = foldl countVowelOrConsonant (0, 0) str
  in  if a > b then Nothing else Just $ Word' str

countVowelOrConsonant :: (Int, Int) -> Char -> (Int, Int)
countVowelOrConsonant (vow, con) x | x `elem` vowels = (vow + 1, con)
                                   | otherwise       = (vow, con + 1)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0     = Nothing
               | n == 0    = Just Zero
               | otherwise = Succ <$> integerToNat (n - 1)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f may = case may of
  Nothing -> z
  Just x  -> f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z = mayybee z id

listToMaybe :: [a] -> Maybe a
listToMaybe []       = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr keepJust []
 where
  keepJust Nothing  acc = acc
  keepJust (Just x) acc = x : acc

flipMaybe' :: [Maybe a] -> [a]
flipMaybe' (x : xs) = case x of
  Nothing -> []
  Just a  -> (a) : (flipMaybe' xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = undefined

lefts' :: [Either a b] -> [a]
lefts' = foldr helper []
 where
  helper (Left a) acc = a : acc
  helper _        acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr helper []
 where
  helper (Right a) acc = a : acc
  helper _         acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr helper ([], [])
 where
  helper (Left  a) (ls, rs) = (a : ls, rs)
  helper (Right b) (ls, rs) = (ls, b : rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either' (const Nothing) (Just . f)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g x = case x of
  Left  a -> f a
  Right b -> g b


myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of
  Just (a, b) -> a : myUnfoldr f b
  Nothing     -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving Show

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z = case f z of
  Just (a, b, c) -> Node (unfold f a) b (unfold f c)
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild 1 = unfold (\_ -> Just (Leaf, 1, Leaf)) Leaf
treeBuild _ = undefined









