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

renameAllFree :: Expr -> Expr -> Int -> Expr
renameAllFree (Var a) (Var var_conv) n | a == var_conv = Var (symbols !! n)
                                       | otherwise = Var a
renameAllFree (App e1 e2) (Var var_conv) n  = App (renameAllFree e1 (Var var_conv) n) (renameAllFree e2 (Var var_conv) n )
renameAllFree (Lambda var1 e) (Var var_conv) n | var1 == var_conv = Lambda var1 e 
                                               | otherwise = Lambda var1 (renameAllFree e (Var var_conv) n)

renameAllBounded :: Expr -> Expr -> Int -> Expr
renameAllBounded (Var a) b n = Var a
-- renameAllBounded (Var a) (Var var_conv) n | a == var_conv = Var (symbols !! n)
                                        --   | otherwise = Var a
renameAllBounded (App e1 e2) (Var var_conv) n  = App (renameAllBounded e1 (Var var_conv) n) (renameAllBounded e2 (Var var_conv) n )
renameAllBounded (Lambda var1 e) (Var var_conv) n | var1 == var_conv = Lambda (symbols !! n) (renameAllFree e (Var var_conv) n)
                                                  | otherwise = Lambda var1 (renameAllBounded e (Var var_conv) n)

alphaConversionNth :: Expr -> Expr -> Int ->(Expr, Expr, Int)
alphaConversionNth (Var a) root n | checkVarFree root (Var a) = (Var (symbols !! n), renameAllBounded root (Var a) n, n+1 )
                                  | otherwise = (Var a, root,  n)
alphaConversionNth (Lambda var expr) root n = alphaConversionNth expr root n
alphaConversionNth (App e1 e2) root n = let (expr', root', n') = alphaConversionNth e1 root n
                                            (expr'', root'',  n'') = alphaConversionNth e2 root n'
                                        in (App expr' expr'', root'', n'')

alphaConversion :: Expr -> Expr
alphaConversion a = second (alphaConversionNth a a 0)

second :: (Expr, Expr, Int) -> Expr
second (a, b, c) = b

-- replaceAllFree :: Expr -> Expr -> Expr -> Expr
-- replaceAllFree (Var replaceWhat) (Var replaceThis) replaceOn | replaceWhat == replaceThis = replaceOn
--                                                              | otherwise = (Var replaceWhat)

-- replaceAllFree (Lambda x (Var y)) (Var replaceThis) replaceOn | x == replaceThis = replaceOn


-- betaReductionNth  expr 
-- betaReductionNth :: Expr -> 

-- betaReduction :: Expr -> Expr
-- betaReduction ()
-- betaReduction (App (Lambda lvar lexpr) (Var vvar)) 

-- eval :: Expr -> Expr
-- eval a = betaReduction(alphaConversion a)

-- main = print( alphaConversion  (App (Lambda "x" (Lambda "y" (App (Var "z") (Var "y")))) (Lambda "y" (App (Var "x") (Lambda "x" (App (Var "z")( Var "y")))))))
-- main = print (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y"))
-- main = print (alphaConversion (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y")))
main = print(alphaConversion (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "x")))

