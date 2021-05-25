import Data.Char

stringToDoubles :: String -> [Double]
stringToDoubles string = map read $ words $ string

normalize :: Int -> [Double] -> [Double]
normalize n lst = let max_val = maximum lst
                      min_val = minimum lst
                    in map (\x -> (fromIntegral (n - 1) * (x - min_val)/max_val) + 1) lst

visualize :: [Int] -> String 
visualize = show

count :: [Double] -> Int -> Int -> Int
count (x:xs) val counter | null xs = counter
                         | maximum (x:xs) == x - 1 = count xs val (counter + 1)
                         | (val <= round x) && (round x < (val + 1)) = count xs val (counter + 1)
                         | otherwise = count xs val counter

frequencies :: Int -> [Double] -> [Int]
frequencies n lst = [(count lst x 0) | x <- [0..n-1]]

histogram :: Int -> String -> IO ()
histogram binCount = putStr . visualize . frequencies binCount . normalize binCount . stringToDoubles


main = do nums <- getLine 
          beans <- read <$> getLine
          histogram beans nums

          
          