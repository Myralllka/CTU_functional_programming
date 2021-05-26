import Data.Char
import Control.Monad

-- derivePolynomial :: Int -> [(Int, Int)] -> [(Int, Int)]
-- derivePolynomial a b = b

(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

strToNum :: String -> String -> String
strToNum (x:xs) res | isDigit x = res ++ [x] ++ strToNum xs res
                    | otherwise = res


readNextNum :: String -> String -> (String, String)
readNextNum (x:xs) res | null (x:xs) = ("0", "")
                       | null xs = if (isDigit x) then (res ++ [x], "") else alt
                       | isAlpha x = alt
                       | isDigit x = readNextNum xs (res ++ [x])
                       | otherwise = ("0", xs)
                    where alt = if null res then ("0", xs) else (res, xs)


count :: String  -> Int -> Int -> String -> String 
count (x:xs) n i res | null xs = if mod (i + 1) n == 0 then res else x : res
                     | mod (i + 1) n == 0 = count xs n (i + 1) res 
                     | otherwise = count xs n (i+1) [x] ++ res
                     
skipEveryKth :: IO String -> Int -> IO String 
skipEveryKth string number = do s <- string
                                return $ reverse (count s number 0 "")

readChar :: IO String
readChar = do putStrLn "Enter char:"
              getLine

readNChars :: Int -> IO String
readNChars n = foldM readEachChar [] (replicate n readChar)
                    where readEachChar result input = do return result +++ input

strToNums :: String -> Int
strToNums "" = 0
strToNums (x:xs) = strToNums b + read a :: Int
                        where (a, b) = readNextNum (x:xs) ""

main = do putStrLn "Enter k:"
          k <- read <$> getLine
          putStrLn "Enter n:"
          n <- read <$> getLine
          tmp <- skipEveryKth (readNChars n) k
          print (strToNums tmp)
