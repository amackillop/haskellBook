module Database where

import           Data.Time
import           Standard

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr unpackTime []
 where
  unpackTime (DbDate val) acc = val : acc
  unpackTime _            acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr unpackNum []
 where
  unpackNum (DbNumber val) acc = val : acc
  unpackNum _              acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = myMaximumBy compare . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = average . map fromIntegral . filterDbNumber

