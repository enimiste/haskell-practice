module Golf where

{-
2i <= n
The returned $list$ should contains :
1 <= i <= n
n = length list
[1:1, 2:2, ...., n:n]
1:1 = [a1, a2, a3, ..., an]
2:2 = [a2, a4, a6, ...., a(2|_n/2_|)]
3:3 = [a3, a6, a9, ...., a(3|_n/3_|)]

i:i = [ai, a(2i), a(3i), ...., a(i|_n/i_|)]

n:n = [an]
-}
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