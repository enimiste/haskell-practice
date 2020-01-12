module Golf (histogram, skips, localMaxima) where

import Histogram

-- SKIPS
skips :: [a] -> [[a]]
skips xs = skips' xs (length xs)
    where   skips' [] _ = []
            skips' ys n = foldl (\b i ->  b ++ [(takeNthOther ys i n [])]) [] [1..n]


takeNthOther :: [a] -> Int -> Int -> [a] -> [a]
takeNthOther ys i n zs = takeNthOther' ys 1 i n zs
    where   takeNthOther' [] _ _ _ zs = zs
            takeNthOther' _ _ 0 _ zs = zs
            takeNthOther' _ j _ _ zs | j > n = zs
            takeNthOther' (y:ys) j i n zs | j `mod` i == 0 = takeNthOther' ys (j + 1) i n (zs ++ [y])
            takeNthOther' (y:ys) j i n zs = takeNthOther' ys (j + 1) i n zs

-- LOCALMAXIMA
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (_:[]) = []
localMaxima (_:_:[]) = []
localMaxima (x:ys@(y:z:xs)) | x < y && y > z = [y] ++ localMaxima ys
localMaxima (_:ys) = localMaxima ys


-- HISTOGRAM
histogram :: [Digit] -> IO ()
histogram xs = showFrequency xs