module Hw3 where

type Symbol = String 
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq 

instance Show Expr where
    show (Var a) = a
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Lambda s e) = "(\\" ++ s ++ "." ++ show e ++ ")"

symbols :: [Symbol]
symbols = ["a" ++ show el | el <- [0..]]

type Dict = [(Char, Int)]

-- alphaConversion :: Expr -> Int -> Bool 
-- alphaConversion (App e1 e2) n = 
-- alphaConversion (App e1 e2) n = 

-- evalNth :: Expr -> Int -> Expr
-- evalNth (Var a) _ = Var a
-- evalNth (Lambda s e) n | 

-- evalNth (App e1 e2) n = App (expand e1 (n+1)) (expand e2 (n+2))
--     -- where expand el' n' | 


-- eval :: Expr -> Expr
-- eval a = evalNth a 0
-- main = print(eval (App (Lambda "x" (Var "x")) (Var "y")))
-- main = print(alphaConversion "x" (App (Lambda "x" (Var "x")) (Var "y")))

