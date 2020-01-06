module TourHanoi where


hello :: String
hello = "Hello world"

type Peg = String -- type alias
type Move = (Peg, Peg) -- Move from the first peg to the second one

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a _ c = [(a, c)]
hanoi nbr a b c = (move (nbr - 1) a b c) ++ [(a, c)] ++ (move (nbr - 1) b c a)

move :: Integer -> Peg -> Peg -> Peg -> [Move]
move nbr from to tmp = []