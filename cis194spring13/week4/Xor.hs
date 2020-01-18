module Xor where

xor' :: [Bool] -> Bool
xor' = odd . length . filter (== True)

xor :: [Bool] -> Bool
xor = foldl1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ e c -> f e : c) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base 
                    . reverse