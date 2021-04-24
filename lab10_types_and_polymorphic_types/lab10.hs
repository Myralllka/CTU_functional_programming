-- Ex 1

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:xs | xs <- interleave x ys]


permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [interleave x p | p <- permutations xs]

main = print (permutations [1,3,4])

-- Ex 2

type Edge a = (a,a)
data Graph a = Graph {vertices :: [a], edges :: [Edge a]} deriving Show
isEdge :: Eq a => Edge a -> Graph a -> Bool
isEdge (a,b) g = (a,b) `elem` edgs || (b,a) `elem` edgs where
    edgs = edges g

isPath :: Eq a => [a] -> Graph a -> Bool
isPath vs g = and [isEdge (vs !! i, vs !! (i+1)) g | i <- [0..length vs-2]]

findHamiltonian :: Eq a => Graph a -> [[a]]
findHamiltonian g = [p | p <- perms, isPath p g]
    where perms = permutations (vertices g)

-- Ex 3. Dual number

data  DualNum a = DN a a deriving (Eq, Ord)

instance Show a => Show (DualNum a) where
        show (DN x x') = show x ++ "+" ++ show x' ++ "eps"

instance Num a => Num (DualNum a) where
    (DN x x') + (DN y y') = DN (x + y) (x' + y')
    (DN x x') - (DN y y') = DN (x - y) (x' - y')
    (DN x x') * (DN y y') = DN (x * y) (x * y' + y * x')
    fromInteger i = DN (fromInteger i) 0
    abs (DN x x') = DN (abs x) (signum x * x')
    signum (DN x _) = DN (signum x) 0 

instance Fractional a => Fractional (DualNum a) where
    (DN x x') / (DN y y') = DN (x / y) ((x' * y - x * y') / (y * y))
    fromRational r = DN (fromRational r) 0

f :: Num a => a -> a
f x = x^2 + 1

-- g :: Fractional a => a -> a
sqr :: (Fractional a, Ord a) => a -> a
sqr x = convAbs $ iterate improve 1
    where improve r = (r + x/r) / 2
          convAbs (x1:x2:xs) | abs (x1 - x2) < 1e-10 = x2
                             | otherwise = convAbs xs


-- Task 1

-- f :: a -> b where b is supposed to be an orderable type and two lists of elements of type a
-- func :: a -> b -> Bool
-- func a b = a > b

merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge f p@(x:xs) q@(y:ys) | f x < f y = x:merge f xs q 
                          | otherwise = y:merge f p ys


-- merge f (ax:axs) (bx:bxs) | f (ax) < f(bx) = concat [ax, bx] merge f axs bxs
                        --   | otherwise = concat [bx, ax] merge f axs bxs