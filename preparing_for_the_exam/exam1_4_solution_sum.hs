mean :: String -> [Double] -> Double
mean variant
  | variant == "harmonic"   = harmonicMean
  | variant == "geometric"  = geometricMean
  | variant == "arithmetic" = arithmeticMean
  | variant == "quadratic"  = quadraticMean

-- printMean :: String -> String -> IO ()

-- harmonicMean as the harmonic mean (also called subcontrary mean) of n numbers

harmonicMean :: [Double] -> Double

-- geometricMean as the geometric mean of n numbers

geometricMean :: [Double] -> Double

-- arithmeticMean as the (standard) arithmetic mean (also called average) of n numbers

arithmeticMean :: [Double] -> Double

-- quadraticMean as the quadratic mean (also called root mean square or rms) of n numbers

quadraticMean :: [Double] -> Double
