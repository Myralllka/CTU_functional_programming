import Data.Char

mean :: String -> [Double] -> Double
mean variant
  | variant == "harmonic"   = harmonicMean
  | variant == "geometric"  = geometricMean
  | variant == "arithmetic" = arithmeticMean
  | variant == "quadratic"  = quadraticMean


-- harmonicMean as the harmonic mean (also called subcontrary mean) of n numbers
harmonicMean :: [Double] -> Double
harmonicMean input = fromIntegral (length input) / sum (map (\x -> 1/x) input)

-- geometricMean as the geometric mean of n numbers
geometricMean :: [Double] -> Double
geometricMean input = p ** (1/pow)
                          where pow = fromIntegral (length input)
                                p = product input

-- arithmeticMean as the (standard) arithmetic mean (also called average) of n numbers
arithmeticMean :: [Double] -> Double
arithmeticMean input = sum input / fromIntegral (length input)

-- quadraticMean as the quadratic mean (also called root mean square or rms) of n numbers

quadraticMean :: [Double] -> Double
quadraticMean input = sqrt (sum (map (\x -> x ** 2) input) / fromIntegral (length input))


stringToDoubles :: String -> [Double]
stringToDoubles str | null str = []
                    | otherwise = fmap read (words str)


printMean :: String -> String -> IO ()
printMean name inp = print (mean name (stringToDoubles inp))

main = do nums <- getLine 
          name <- getLine 
          printMean name nums
          