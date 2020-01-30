module CreditCardCheck where

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (toDigits $ div n 10) ++ [rem n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (rem n 10) : (toDigitsRev $ div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list@(x:xs)
  | even len = doubleEveryOther' list
  | otherwise = x : doubleEveryOther' xs
  where
    len = length list
    doubleEveryOther' [] = []
    doubleEveryOther' (x:y:ys) = x * 2 : y : doubleEveryOther' ys

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = (==) 0 . (flip rem) 10 . sumDigits . doubleEveryOther . toDigits