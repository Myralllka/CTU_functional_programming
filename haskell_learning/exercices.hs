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


-- Ex 6
