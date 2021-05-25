import Data.Char
import Control.Monad

derivePolynomial :: Int -> [(Int, Int)] -> [(Int, Int)]
derivePolynomial a b = b

(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

strToNum :: String -> String -> String
strToNum (x:xs) res | isDigit x = res ++ [x] ++ strToNum xs res
                    | otherwise = res

str2ToNum :: String -> String -> String
str2ToNum (x:xs) res | ',' == x = strToNum xs res
                     | otherwise = str2ToNum xs res

strToPair :: String -> [(Int, Int)]
strToPair (x:xs) = let a = strToNum xs ""
                       b = str2ToNum xs ""
                    in [(read a, read b)]


readPolynomial :: Int -> IO [(Int, Int)]
readPolynomial n = foldM readEach [] (replicate n promt)
                    where readEach result input = do 
                                return result +++ input
                          promt = do putStrLn "Enter term as a pair of the form (coef, exp):"
                                     strToPair <$> getLine

main = do putStrLn "Enter number of non-zero terms:"
          n <- read <$> getLine
          readPolynomial n

