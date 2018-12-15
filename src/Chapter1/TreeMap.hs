module Chapter1.TreeMap where

import Prelude hiding (lookup)

data TreeMap k v = Leaf | Tree (TreeMap k v) k v (TreeMap k v)
  deriving (Eq, Show)

insert :: Ord k => k -> v -> TreeMap k v -> TreeMap k v
insert key value Leaf = Tree Leaf key value Leaf
insert key value (Tree l k v r) 
  | key < k   = Tree (insert key value l) k v r
  | key > k   = Tree l k v (insert key value r)
  | otherwise = Tree l key value r

lookup :: Ord k => k -> TreeMap k v -> Maybe v
lookup _ Leaf = Nothing  
lookup key (Tree l k v r)
  | key < k   = lookup key l 
  | key > k   = lookup key r 
  | otherwise = Just v  
