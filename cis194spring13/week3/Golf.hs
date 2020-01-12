module Golf (frequency, histogram, showHistogram, skips, localMaxima, Digit, Freq) where

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
type Digit = Integer
type Freq = Integer

data BTree a = Leaf
               | Node (BTree a) a (BTree a)
            deriving (Show)

type DigitFreqBTree = BTree (Digit, Freq)
type TreeElemMerge = (Digit, Freq) -> (Digit, Freq) -> (Digit, Freq) 
type TreeElemComp = (Digit, Freq) -> (Digit, Freq) -> Ordering

-- Main function
showFrequency :: [Digit] -> IO ()
showFrequency = (putStr . showHistogram . histogram . frequency)

frequency :: [Digit] -> [(Digit, Freq)]
frequency [] = []
frequency xs = sortDigitFreqAsc initDigits (toFreq 1 xs)

showHistogram :: [String] -> String
showHistogram = foldr (\ s acc-> s ++ "\n" ++ acc) ""

histogram :: [(Digit, Freq)] -> [String]
histogram [] = ["==========", "0123456789"]
histogram xs = histogram' (["==========", "0123456789"], Just xs)
    where   histogram' (ss, Nothing) = ss   
            histogram' (ss, Just freqs) = histogram' (makeAsterisks ss freqs)

makeAsterisks :: [String] -> [(Digit, Freq)] -> ([String], Maybe [(Digit, Freq)])
makeAsterisks ss [] = (ss, Nothing)
makeAsterisks ss xs = ((makeAsterisks' "" xs) : ss, allZeros (decrement xs))
    where   makeAsterisks' s [] = s
            makeAsterisks' s ((d, f):ys) | f > 0 = "*" ++ (makeAsterisks' s ys)
            makeAsterisks' s ((d, f):ys) | f == 0 = " " ++ (makeAsterisks' s ys)

            decrement [] = []
            decrement ((d, f):ys) | f > 0 = (d, f-1) : decrement ys
            decrement (x:ys) = x : decrement ys

            allZeros [] = Nothing
            allZeros xs | all (\ (d, f) -> f == 0) xs = Nothing
            allZeros xs = Just xs


-- Sort by Digit in asc order
sortDigitFreqAsc :: BTree (Digit, Freq) -> [(Digit, Freq)] -> [(Digit, Freq)]
sortDigitFreqAsc init = toList . foldl (\ t d -> insertByDigit t d) init

toFreq :: Integer -> [Digit] -> [(Digit, Freq)]
toFreq x = map (\ d -> (d, x))

initDigits :: DigitFreqBTree
initDigits = foldl (\ t d -> insertByDigit t d) (Node Leaf (5, 0) Leaf) (toFreq 0 [0..9])

insertByDigit :: BTree (Digit, Freq) -> (Digit, Freq) -> BTree (Digit, Freq)
insertByDigit tree x = insertBy byDigit tree x
    where   byDigit (d1, _) (d2, _) | d1 == d2 = EQ
            byDigit (d1, _) (d2, _) | d1 < d2 = LT
            byDigit _ _ = GT

insertBy :: TreeElemComp -> BTree (Digit, Freq) -> (Digit, Freq) -> BTree (Digit, Freq)
insertBy f Leaf x = Node Leaf x Leaf
insertBy f (Node l e r) x | (f x e) == EQ = Node l (mergeElem x e) r
insertBy f (Node l e r) x | (f x e) == LT = Node (insertBy f l x) e r
insertBy f (Node l e r) x = Node l e (insertBy f r x)

mergeElem :: TreeElemMerge
mergeElem (d1, f1) (d2, f2) | d1 == d2 = (d1, f1 + f2)

-- in Order
toList :: BTree a -> [a]
toList Leaf = []
toList (Node l e r) = (toList l) ++ [e] ++ (toList r)
