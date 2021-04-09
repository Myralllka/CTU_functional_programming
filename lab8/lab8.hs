-- Ex 1
separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = let (evs, ods) = separate xs
                    in (x:evs, y:ods)
-- Ex 2
numToStr :: Int -> Int -> String
numToStr n radix = if  n < radix then [chars !! n]
                   else (numToStr d radix) ++ [chars !! r]
                 where chars = ['0'..'9'] ++ ['A'..'F']
                       d = n `div` radix
                       r = n `mod` radix
-- Ex 3
split :: Int -> [Int] -> [[Int]]
split n xs | (length xs) <= n = [xs]
           | otherwise = take n xs : (split n (drop n xs))


average_n :: Int -> [Int] -> [Float]
average_n n ys = [fromIntegral (sum xs) / fromIntegral (length xs) | xs <- xss]
    where xss = split n ys

-- Task 1
{-
 - Write a function copy :: Int -> String -> String that takes an integer n and a string str and returns a string consisting of n copies of string
 - -}
copy :: Int -> String -> String
copy 0 _ = ""
copy n src | n == 1 = src
           | otherwise = src ++ copy (n-1) src


-- Task 2
{-The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:

consider each digit as a separate number;
moving left, double every other number from the second last, e.g. 1 7 8 4 â 2 7 16 4;
subtract 9 from each number that is now greater than 9;
add all the resulting numbers together;
if the total is divisible by 10, the card number is valid.
 -
 - -}
luhnDouble :: Int -> Int
luhnDouble n = 2 * n `mod` 9

luhn :: [Int] -> Bool 
luhn xs = (sum evs + sum [luhnDouble x | x <- ods]) `mod` 10 == 0
    where rxs = reverse xs
          (evs, ods) = separate rxs

main = print ( luhn [1,7,8,4])

