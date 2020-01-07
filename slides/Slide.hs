module Slide (Expr, eval, pyth, Student) where


-- DATA TYPES
data Expr a = Const a
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)
    | Neg (Expr a)

type IntExpr = Expr Integer

eval :: IntExpr -> Integer
eval e =
    case e of
        Const c -> c
        Neg e1 -> (-1) * eval e1
        Add e1 e2 -> eval e1 + eval e2
        Mult e1 e2 -> eval e1 * eval e2
        Sub e1 e2 -> eval e1 - eval e2

evalNum :: Num a => Expr a -> a
evalNum e =
    case e of
        Const c -> c
        Neg e1 -> (-1) * evalNum e1
        Add e1 e2 -> evalNum e1 + evalNum e2
        Mult e1 e2 -> evalNum e1 * evalNum e2
        Sub e1 e2 -> evalNum e1 - evalNum e2

-- List comprehension
pyth :: Integer -> Integer -> [(Integer, Integer, Integer)]
pyth b n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, y > b]


-- QUALIFIED TYPES
data Student = Student Name Score
type Name = String
type Score = Integer

better :: Student -> Student -> Bool
better (Student _ s1) (Student _ s2) = s1 > s2

instance Show Student where
    show (Student name score) = "Student(name=" ++ name ++ ", score=" ++ (show score) ++ ")"

instance Eq Student where
    (Student name1 s1) == (Student name2 s2) = name1 == name2
    (Student name1 s1) /= (Student name2 s2) = name1 /= name2

instance Ord Student where
    s1 > s2 = better s1 s2
    s1 <= s2 = not (better s1 s2) 