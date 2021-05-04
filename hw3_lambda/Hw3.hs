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

checkVarFree :: Expr -> Expr -> Bool
checkVarFree (Var a) (Var var_conv) | a == var_conv = True
                                | otherwise = False
checkVarFree (App e1 e2) (Var var_conv) = checkVarFree e1 (Var var_conv) || checkVarFree e2 (Var var_conv)
checkVarFree (Lambda var1 e) (Var var_conv) | var1 == var_conv = False
                                            | otherwise = checkVarFree e (Var var_conv)

renameAllFree :: Expr -> Expr -> Int -> Expr
renameAllFree (Var a) (Var var_conv) n | a == var_conv = Var (symbols !! n)
                                       | otherwise = Var a
renameAllFree (App e1 e2) (Var var_conv) n  = App (renameAllFree e1 (Var var_conv) n) (renameAllFree e2 (Var var_conv) n )
renameAllFree (Lambda var1 e) (Var var_conv) n | var1 == var_conv = Lambda var1 e 
                                               | otherwise = Lambda var1 (renameAllFree e (Var var_conv) n)

renameAllBounded :: Expr -> Expr -> Int -> Expr
renameAllBounded (Var a) b n = Var a
renameAllBounded (App e1 e2) (Var var_conv) n  = App (renameAllBounded e1 (Var var_conv) n) (renameAllBounded e2 (Var var_conv) n )
renameAllBounded (Lambda var1 e) (Var var_conv) n | var1 == var_conv = Lambda (symbols !! n) (renameAllFree e (Var var_conv) n)
                                                  | otherwise = Lambda var1 (renameAllBounded e (Var var_conv) n)

alphaConversionNth :: Expr -> Expr -> Int -> (Expr, Expr, Int)
alphaConversionNth (Var a) root n | checkVarFree root (Var a) && not (isSymbol a) = (Var (symbols !! n), renameAllBounded root (Var a) n, n + 1 )
                                  | otherwise = (Var a, root,  n)
alphaConversionNth (Lambda var expr) root n = alphaConversionNth expr root n
alphaConversionNth (App e1 e2) root n = let (expr', root', n') = alphaConversionNth e1 root n
                                            (expr'', root'',  n'') = alphaConversionNth e2 root' n'
                                        in (App expr' expr'', root'', n'')

alphaConversion :: Expr -> Expr
alphaConversion a = second (alphaConversionNth a a 0)

second :: (Expr, Expr, Int) -> Expr
second (a, b, c) = b

-----------------------------------------------------------------------------------------

replaceAllFree :: Expr -> Expr -> Expr -> Expr
replaceAllFree (Var x) (Var replaceWhat) replaceOn | x == replaceWhat = replaceOn
                                                   | otherwise = Var x
replaceAllFree (Lambda x expr) (Var replaceWhat) replaceOn | x == replaceWhat = Lambda x expr
                                                           | otherwise = Lambda x (replaceAllFree expr (Var replaceWhat) replaceOn)        
replaceAllFree (App (Var x) (Var y)) (Var replaceWhat) replaceOn = App (replaceAllFree (Var x)(Var replaceWhat) replaceOn) (replaceAllFree (Var y) (Var replaceWhat) replaceOn)
replaceAllFree (App (Lambda x expr) e2) (Var replaceWhat) replaceOn = betaReduction(replaceAllFree expr (Var x) e2)

replaceAllFree (App e1 e2) (Var replaceWhat) replaceOn = betaReduction (App (replaceAllFree e1 (Var replaceWhat) replaceOn) (replaceAllFree e2 (Var replaceWhat) replaceOn))



-- replaceAllFree (App e1 e2) (Var replaceWhat) replaceOn = App (replaceAllFree e1 (Var replaceWhat) replaceOn) (replaceAllFree e2 (Var replaceWhat) replaceOn)

betaReduction :: Expr -> Expr
-- betaReduction (Var a) = Var a
betaReduction (App (Var x) (Var y)) = App (Var x) (Var y)
betaReduction (App (Lambda x expr) e2) = betaReduction (replaceAllFree expr (Var x) e2)
betaReduction (Lambda x expr) = Lambda x (betaReduction expr)
betaReduction (App e1 e2) = App (betaReduction e1) (betaReduction e2)
-- betaReduction (App e1 (Var z)) = replaceAllFree e1 (Var z) (Var z)
betaReduction a = a

eval :: Expr -> Expr
eval a = betaReduction(alphaConversion a)



suc = Lambda "w" (Lambda "y" (Lambda "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))
-- one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))

main = print (eval (App (Lambda "z" (Lambda "s" (Lambda "z" (App (Var "s") (Var "z"))))) (App (Var "s") (Var "z"))))