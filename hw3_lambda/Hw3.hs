module Hw3 where

type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var a) = a
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Lambda s e) = "(\\" ++ s ++ "." ++ show e ++ ")"

symbols :: [Symbol]
symbols = ["a" ++ show el | el <- [0..]]


checkVarFree :: Expr -> Expr -> Bool
checkVarFree (Var a) (Var var_conv) | a == var_conv = True
                                | otherwise = False
checkVarFree (App e1 e2) (Var var_conv) = checkVarFree e1 (Var var_conv) || checkVarFree e2 (Var var_conv)
checkVarFree (Lambda var1 e) (Var var_conv) | var1 == var_conv = False
                                            | otherwise = checkVarFree e (Var var_conv)


replaceAllFree :: Expr -> Expr -> Int -> Expr
replaceAllFree (Var a) (Var var_conv) n | a == var_conv = Var (symbols !! n)
                                        | otherwise = Var a
replaceAllFree (App e1 e2) (Var var_conv)n  = App (replaceAllFree e1 (Var var_conv) n) (replaceAllFree e2 (Var var_conv) n )
replaceAllFree (Lambda var1 e) (Var var_conv) n | var1 == var_conv = Lambda var1 e
                                                | otherwise = Lambda var1 (replaceAllFree e (Var var_conv) n)

alphaConversionNth :: Expr -> Int -> (Expr, Int)
alphaConversionNth (Var a) n = (Var a, n)
alphaConversionNth (App e1 e2) n = let (expr', n') = alphaConversionNth e1 n
                                       (expr'', n'') = alphaConversionNth e2 n'
                                            in (App expr' expr'', n'')

alphaConversionNth (Lambda var expr) n = let (Lambda v root, n') | checkVarFree expr (Var var) = (Lambda (symbols !! n) (replaceAllFree expr (Var var) n ), n + 1 )
                                                                 | otherwise = (Lambda var expr, n)
                                             (newExpr, n'') = alphaConversionNth root n'
                                            in (Lambda v newExpr, n'')

alphaConversion :: Expr -> Expr
alphaConversion a = fst (alphaConversionNth a 0)



main = print( alphaConversion  (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Lambda "y" (App (Var "x") (Lambda "x" (App (Var "z")( Var "y")))))))
-- main = print  (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y"))

