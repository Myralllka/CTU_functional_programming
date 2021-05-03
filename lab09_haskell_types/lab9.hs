-- Ex 1

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show (Leaf x) = "<Leaf " ++ show x ++ "/>"
    show (Node left right) = "<Node>" ++ show left ++ show right ++ "</Node>"

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

-- Ex 2

tree_depth :: Tree a -> Int
tree_depth (Leaf _) = 1
tree_depth (Node left right) = 1 + max (tree_depth left) (tree_depth right)

-- Ex 3

labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
labelHlp (Leaf x) n = (Leaf (x, n), n + 1)
labelHlp (Node left right) n = let (left', n') = labelHlp left n
                                   (right', n'') = labelHlp right n' 
                                 in (Node left' right', n'')

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (labelHlp t 0)

-- Task 1

type Monomial a = (a, Int)

format :: (Show a, Ord a, Num a) => Monomial a -> String
format (0, _) = "0"
format (ml, 0) = show ml
-- format (ml, deg) = show ml ++ "*x^" ++ show deg
format (ml, deg) | deg == 0 = display ml
                 | otherwise = display ml ++ "x*^" ++ show deg
                 where display k | k >= 0 = show k
                                 | otherwise = "(" ++ show k ++ ")"

data Polynomial a = Null | Pol (Monomial a) (Polynomial a)

instance (Show a, Ord a, Num a) => Show (Polynomial a) where
    show (Pol m Null) = format m
    show (Pol mon pol) = format mon ++ "+" ++ show pol
    show Null = "0"



pol::Polynomial Int
pol = Pol (-1, 0) (Pol (2, 23) (Pol (-1, 3) Null))

-- Task 2

get_degree :: Polynomial a -> Int
get_degree p = iter p (-1) where
            iter Null n = n
            iter (Pol (_, e) ms) n | e > n = iter ms e
                                   | otherwise = iter ms n


main = print (get_degree pol)