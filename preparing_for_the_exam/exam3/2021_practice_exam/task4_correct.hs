import Data.List
data Term a = Nil | Term (a, Char, Integer) deriving Eq
data Polynom a = Pol [Term a]
instance (Show a, Num a, Ord a) => Show (Term a) where
    show Nil = ""
    show (Term (c, _, 0)) = (show c)
    show (Term (c, v, 1)) | c < 0 = "(" ++ (show c) ++ ")*" ++ [v]
                          | otherwise = (show c) ++ "*" ++ [v]
    show (Term (c, v, e)) | c < 0 = "(" ++ (show c) ++ ")*" ++ [v] ++ "^" ++ (show e)
                          | otherwise = (show c) ++ "*" ++ [v] ++ "^" ++ (show e)
join :: String -> [String] -> String
join _ [x] = x
join sep (x:xs) = x ++ sep ++ (join sep xs)
instance (Show a, Num a, Ord a) => Show (Polynom a) where
    show (Pol ts) = join " + " [show t | t <- ts] 

derivTerm :: Num a => Term a -> Term a
derivTerm Nil = Nil
derivTerm (Term (c, v, e)) | e == 0 = Nil
                           | otherwise = Term (c*(fromInteger e), v, e-1)

derivPol 0 (Pol ts) = (Pol ts)
derivPol n (Pol ts) = derivPol (n-1) (Pol (filter (/= Nil) (map derivTerm ts)))

readTerms :: Int -> (Polynom Int) -> IO (Polynom Int)
readTerms 0 p = return p
readTerms k (Pol ts) = do putStrLn "Enter term as a pair of the form (coef, exp)"
                          l <- getLine
                          let (c,e) = (read l) :: (Int, Integer)
                          readTerms (k-1) (Pol ((Term (c,'x',e)):ts))
sortPol :: Eq a => Polynom a -> Polynom a
sortPol (Pol ts) = Pol (sortBy (\(Term (_,_,e1)) (Term (_,_,e2)) -> compare e1 e2) qs) where
    qs = filter (/= Nil) ts
main :: IO()
main = do putStrLn "Enter number of non-zero terms:"
          l <- getLine
          let n = (read l) :: Int
          p <- readTerms n (Pol [])
          putStrLn "Enter number of derivative:"
          dline <- getLine
          let d = (read dline) :: Int
          putStrLn (show (sortPol (derivPol d (sortPol p))))
