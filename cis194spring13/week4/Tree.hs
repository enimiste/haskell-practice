module Tree where

type Height = Integer

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
            deriving (Show)


foldTree :: [a] -> Tree a
foldTree = foldr (flip insert) Leaf

insert :: Tree a -> a -> Tree a
insert Leaf e = Node 0 Leaf e Leaf
insert (Node _ l v r) e
    | treeHeight l > treeHeight r = Node (treeHeight newRight + 1) l v newRight
    | otherwise = Node (treeHeight newLeft + 1) newLeft v r
    where   newLeft = insert l e
            newRight = insert r e

treeHeight :: Tree a -> Height
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h