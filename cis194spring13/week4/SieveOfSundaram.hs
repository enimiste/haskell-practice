module SieveOfSundaram where

import Data.List

{-|
Returns prime numbers Up to 2*(n+1)
-}
sieveSundaram' :: Integer -- ^ Input n
                 -> [Integer] -- ^ Primes Up to 2*n+2
sieveSundaram' n = map (\x -> 2*x + 1) remNbrs
    where remNbrs = [1..n] \\ (nbrsOfForm n)


nbrsOfForm :: Integer -> [Integer]
nbrsOfForm n = [ i + j + 2*i*j | i <- [1..n], j <- [i..n], i + j + 2*i*j <= n]


-- | Compute all cartesian products of the input lists
cartProd :: [a] -- ^ First list
         -> [b] -- ^ Second list
         -> [(a, b)] -- ^ Cartesian product
cartProd xs ys = [(x, y) | x <- xs, y <- ys]