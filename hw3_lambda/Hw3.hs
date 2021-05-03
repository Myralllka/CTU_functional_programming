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


replaceAllFree :: Expr -> Expr -> Bool
replaceAllFree (Var a) (Var var_conv) | a == var_conv = (Var var_conv)
                                      | otherwise = (Var a)
replaceAllFree (App e1 e2) (Var var_conv) = replaceAllFree e1 (Var var_conv) || replaceAllFree e2 (Var var_conv)
replaceAllFree (Lambda var1 e) (Var var_conv) | var1 == var_conv = False
                                              | otherwise = replaceAllFree e (Var var_conv)

--

alphaConversionNth :: Expr -> Expr -> Int -> (Expr, Int)
alphaConversionNth (Var a) (Var var_conv) n | a == var_conv = (Var (symbols !! n), n)
                                            | otherwise = (Var a, n)
alphaConversionNth (App e1 e2) var_conv n = let (expr', n') = alphaConversionNth e1 var_conv n
                                                (expr'', n'') = alphaConversionNth e2 var_conv n'
                                            in (App expr' expr'', n'')
alphaConversionNth (Lambda var expr) var_conv n | 

alphaConversion :: Expr -> Expr
alphaConversion a = fst (alphaConversionNth a (Var "") 0)

main = print( alphaConversion (App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y")))

