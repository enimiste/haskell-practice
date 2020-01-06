module CreditCard where
    
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n <= 9 = [n]
    | otherwise = toDigits (div n 10) ++ [mod n 10] 


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n <= 9 = [n]
    | otherwise = mod n 10 : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = doubleEveryOtherHelper (len xs) xs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
    | mod x 10 == x = x + sumDigits xs
    | otherwise = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = 
    let
        ns = toDigits n
        dns = doubleEveryOther ns
        sm = sumDigits dns
    in
        mod sm 10 == 0


-- HELPERS FUNCTIONS

doubleEveryOtherHelper :: Integer -> [Integer] -> [Integer]
doubleEveryOtherHelper 0 [] = []
doubleEveryOtherHelper 1 (x : []) = [x]
doubleEveryOtherHelper n (x : (y : xs)) 
    | mod n 2 == 0 = (2 * x) : y : doubleEveryOtherHelper (n - 2) xs
    | otherwise = x : (2 * y) : doubleEveryOtherHelper (n - 2) xs

len :: [Integer] -> Integer
len [] = 0
len (x : xs) = 1 + len xs