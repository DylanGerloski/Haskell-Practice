toDigits :: Integral n => n -> [n]
toDigits 0 = []
toDigits n
 | n < 0 = []
 | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
 | n < 0 = []
 |otherwise = n `mod` 10 : toDigitsRev (n `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = x:[]
doubleEveryOther (x:(y:zs)) = x : (y * 2) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) | x > 9 = (1 + x `mod` 10) + sumDigits xs
                 | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate n
 | sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0 = True 
 | otherwise = False
