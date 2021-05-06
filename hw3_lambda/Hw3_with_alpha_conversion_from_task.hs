module Hw3 where

import Text.Read

type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var a) = a
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Lambda s e) = "(\\" ++ s ++ "." ++ show e ++ ")"

symbols :: [Symbol]
symbols = ["a" ++ show el | el <- [0..]]

isSymbol :: Symbol -> Bool 
isSymbol (x:xs) = let tmp = case readMaybe xs :: Maybe Int of
                            Just x -> True
                            Nothing -> False
                    in tmp && x == 'a'

isVarFree :: Expr -> Expr -> Bool
isVarFree (Var a) (Var var_conv) | a == var_conv = True
                                     | otherwise = False
isVarFree (App e1 e2) (Var var_conv) = isVarFree e1 (Var var_conv) || isVarFree e2 (Var var_conv)
isVarFree (Lambda var1 e) (Var var_conv) | var1 == var_conv = False
                                         | otherwise = isVarFree e (Var var_conv)

renameAllFree :: Expr -> Expr -> Int -> Expr
renameAllFree (Var a) (Var var_conv) n | a == var_conv = Var (symbols !! n)
                                       | otherwise = Var a
renameAllFree (App e1 e2) (Var var_conv) n  = App (renameAllFree e1 (Var var_conv) n) (renameAllFree e2 (Var var_conv) n )
renameAllFree (Lambda var1 e) (Var var_conv) n | var1 == var_conv = Lambda var1 e 
                                               | otherwise = Lambda var1 (renameAllFree e (Var var_conv) n)

renameAllFreeInLambda :: Expr -> Expr -> Int -> Expr
renameAllFreeInLambda (Lambda x expr) var_conv n = renameAllFreeInLambda expr var_conv n
renameAllFreeInLambda a b n = renameAllFree a b n

isVarInLambdaFree :: Expr -> Expr -> Bool
isVarInLambdaFree (Lambda x expr) var = isVarFree expr var
isVarInLambdaFree a var = isVarFree a var

renameAllFreeFromExpr :: Expr -> Expr -> Int -> (Expr, Int)
renameAllFreeFromExpr expr_to_rename_from (Var var_conv) n | isVarFree expr_to_rename_from (Var var_conv) = (renameAllFree expr_to_rename_from (Var var_conv) n, n+1)
                                                           | otherwise = (expr_to_rename_from, n)
renameAllFreeFromExpr expr_to_rename_from (App e1 e2) n = let (expr', n') = renameAllFreeFromExpr expr_to_rename_from e1 n
                                                              (expr'', n'') = renameAllFreeFromExpr expr' e2 n'
                                                            in (App expr' expr'', n'')
renameAllFreeFromExpr expr_to_rename_from (Lambda x expr) n = betaReductionNth expr 

betaReductionNth ::Expr -> Expr -> Expr -> Int -> (Expr, Int)
betaReductionNth (Var x) (Var replaceWhat) replaceOn n | x == replaceWhat = (replaceOn, n)
                                                       | otherwise = (Var x, n)
betaReductionNth (App e1 e2) replaceWhat replaceOn n = let (e', n') = betaReductionNth e1 replaceWhat replaceOn n
                                                           (e'', n'') = betaReductionNth e2 replaceWhat replaceOn n'
                                                        in (App e' e'', n'')
betaReductionNth (Lambda x expr) replaceWhat replaceOn n | Var x == replaceWhat = (Lambda x expr, n)
                                                         | otherwise = let (a1, a2) = renameAllFreeFromExpr expr replaceWhat n
                                                                     in betaReduction a1 a2

betaReduction :: Expr -> Int -> (Expr, Int)
betaReduction (Var a) n = (Var a, n)
betaReduction (App (Lambda x expr) e2) n = let (expr', n') = renameAllFreeFromExpr expr e2 n
                                          in betaReductionNth expr' (Var x) e2 n'
betaReduction (App e1 e2) n = let (e', n') = betaReduction e1 n
                                  (e'', n'') = betaReduction e2 n'
                               in (App e' e'', n'')
betaReduction (Lambda x expr) n = (Lambda x (fst (betaReduction expr n)), n+1)

-- betaReduction (Lambda x expr) n | checkIfFreeBecomesBound expr (Var x) = (expr, n)
--                                 | otherwise = (Lambda x expr, n)

-- betaReduction (App (Lambda x expr) e2) n = 

-- eval :: Expr -> Expr
-- eval a = betaReduction(alphaConversion a)

-- suc = Lambda "w" (Lambda "y" (Lambda "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))
one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
two =  App (Lambda "z" one) (App (Var "s") (Var "z"))
-- 
-- App (Lambda "z" (Lambda "s" (Lambda "z" (App (Var "s") (Var "z"))))) (App (Var "s") (Var "z"))

main = print (isVarInLambdaFree one (Var "s"))