module Slide where


-- DATA TYPES
data Expr a = Const a
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)
    | Neg (Expr a)


eval :: Expr Integer -> Integer
eval e =
    case e of
        Const c -> c
        Neg e1 -> (-1) * eval e1
        Add e1 e2 -> eval e1 + eval e2
        Mult e1 e2 -> eval e1 * eval e2
        Sub e1 e2 -> eval e1 - eval e2
