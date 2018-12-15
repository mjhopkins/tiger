module Chapter1.BinarySearchTree where

type Key = String

data Tree = Leaf | Tree Tree Key Tree
  deriving (Eq, Show)

empty :: Tree
empty = Leaf  

insert :: Key -> Tree -> Tree
insert key Leaf = Tree Leaf key Leaf 
insert key (Tree l k r) 
   | key < k   = Tree (insert key l) k r
   | key > k   = Tree l k (insert key r)
   | otherwise = Tree l key r

member :: Key -> Tree -> Bool
member _ Leaf = False  
member key (Tree l k r) 
  | key < k = member key l
  | key > k = member key r
  | otherwise = key == k
  
tree1, tree2 :: Tree  
tree1 = foldl (flip insert) Leaf (map (:[]) "tspipfbst")  
tree2 = foldl (flip insert) Leaf (map (:[]) "abcdefghi")  
