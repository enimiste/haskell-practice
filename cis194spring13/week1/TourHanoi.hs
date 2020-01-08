module TourHanoi where


hello :: String
hello = "Hello world"

type Peg = String -- type alias
type Move = (Peg, Peg) -- Move from the first peg to the second one

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a _ c = [(a, c)]
hanoi nbr a b c = (move (nbr - 1) a b c) ++ [(a, c)] ++ (move (nbr - 1) b c a)
    where   move 1 from to tmp = [(from, to)]
            move 2 from to tmp = [(from, to), (from, tmp), (tmp, to)]
            move nbr from to tmp = (move (nbr - 1) from tmp to) ++ [(from, to)] ++ (move (nbr - 1) tmp to from)


len :: [a] -> Integer
len = len' 0
    where len' n [] = n
          len' n (_ : xs) = n `seq` len' (n+1) xs

heead:: [a] -> Maybe a
heead [] = Nothing
heead (x:_) = Just x