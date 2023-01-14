module Chapter10 where

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop =
  let stops = "pbtdkg"
  in  [ (s1, v, s2) | s1 <- stops, v <- "aeiou", s2 <- stops ]

