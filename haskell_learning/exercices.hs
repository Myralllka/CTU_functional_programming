power :: Integer -> Integer -> Integer
power _ 0 = 1
power n k = n * power n (k-1)


-- Ex 1. separate List 

separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = let (evs, ods) = separate xs
                    in (x:evs, y:ods)

-- Ex 2. 

numToStr :: Int -> Int -> String
numToStr n radix | n < radix = [chars !! n]
                 | otherwise  = numToStr d radix ++ [chars !! r]
                    where chars = ['0'..'9'] ++ ['A'..'F']
                          d = n `div` radix
                          r = n `mod` radix


-- Ex 3

split :: Int -> [Int] -> [[Int]]
split n xs | n > length xs = [xs]
           | otherwise = take n xs : split n (drop n xs)

averageN :: Int -> [Int] -> [Float]
averageN n ys = [fromIntegral (sum xs) / fromIntegral(length xs) | xs <- xss]
                where xss = split n ys

-- Ex 4

copy :: Int -> String -> String
copy n str | n <= 0 = ""
           | otherwise = str ++ copy (n-1) str

-- Ex 5

luhnDouble :: Int -> Int
luhnDouble n = n * 2 `mod` 9

luhn :: [Int] -> Bool
luhn xs = (sum evs + sum [luhnDouble x | x <- ods]) `mod` 10 == 0
        where rxs = reverse xs
              (evs, ods) = separate rxs

-- HackerRank exercises

printHello :: Int -> String
printHello n | n < 2 = "HelloWorld"
             | otherwise = printHello (n - 1) ++ "HelloWorld\n"


-- copy array
copyArray :: Int -> [Int] -> [Int]
copyArray n (x:xs) | null xs = k
                   | n <= 0 = x:xs
                   | otherwise = k ++ copyArray n xs
                    where k = [x | i <- [1..n]]

-- filter array
fltr :: Int -> [Int] -> [Int]
fltr n (x:xs) | null xs = [x | x <= n]
              | x <= n = x : fltr n xs
              | otherwise = fltr n xs

--
f :: [Int] -> [Int]
f [] = []
f [x] = []
f [x, y] = [y]
f (x:y:xs) = y : f xs


-- reverse
rev :: [Int] -> [Int]
rev (x:xs) | null xs = [x]
           | otherwise = rev xs ++ [x]


-- Lecture 
-- Algebraic data types

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]
flip ::Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y


safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

data Person = Person {firstName :: String,
                       lastName :: String,
                            age :: Int,
                          phone :: String}
defaultPerson = Person {lastName = "Morh", 
                       firstName = "Mykola", 
                             age = 20, 
                           phone = "+99"}

-- main = print (firstName defaultPerson)

data List a = Nil | Cons a (List a)
instance Show a => Show (List a) where
    show lst = "<" ++ disp lst ++ ">" where
        disp Nil = ""
        disp (Cons x Nil) = show x
        disp (Cons x l) =  show x ++ ", " ++ disp l

-- Ex 6. Tree

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show (Leaf a) = "<Leaf " ++ show a ++ "/>"
    show (Node left right) = "<Node>" ++ show left ++ show right ++ "</Node>"

treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node left right) = 1 + max (treeDepth left) (treeDepth right)

labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
labelHlp (Leaf x) n = (Leaf (x, n), n + 1)
labelHlp (Node left right) n = let (left', n') = labelHlp left n 
                                   (right', n'') = labelHlp right n'
                                in (Node left' right', n'')


labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (labelHlp t 0)


tree :: Tree Char
tree = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

-- Ex 7. Polynomial

type Monomial a = (a, Int)

data Polynomial a = Null | Pol (Monomial a) (Polynomial a)

format :: (Show a, Ord a, Num a) => Monomial a -> String
format (a, b) | a == 0 = show 0
              | b == 0 = show (signum a)
              | otherwise =  display a ++ show "x^" ++ display b
            where display k | k < 0 = "(" ++ show k ++ ")"
                            | otherwise = show k

instance (Show a, Num a, Ord a) => Show (Polynomial a) where
    show Null = "0"
    show (Pol m Null) = format m
    show (Pol m ms) = format m ++ "+" ++ show ms

getDegree :: Polynomial a -> Int 
getDegree Null = -1
getDegree (Pol (_, e) ms) = max e (getDegree ms)

p :: Polynomial Int
p = Pol (-4, 0) (Pol (-2, 1) (Pol (1, 4) Null))

-- Ex 7. permutations

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:xs | xs <- interleave x ys]
        -- 4  1 234     4 1 234    1 [interleave 4 234]

-- permutations :: a -> [a] -> [[a]]
-- permutations 

main = print (interleave 4 [1, 2, 3, 4])
